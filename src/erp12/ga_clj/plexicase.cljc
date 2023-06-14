(ns erp12.ga-clj.plexicase
  (:require [erp12.ga-clj.toolbox :as tools])
  (:require (bigml.sampling [simple :as simple]
                                  [reservoir :as reservoir]
                                  [stream :as stream]))
  )

"TODO
 - check if its plexicase calculations that are slow - yes, 3-8 sec, when errors get to be mostly 0s, it's slower on the order of 15-20 seconds
 - if so, might need to reimplement using a matrix library
 - either way, ask Li how much time was spent on plexicase calculations in Python implementation
 - finalize make-plexicase-selection, see TODO in it below
 "

(defn calculate-elitist-on-case
  "Calculates the hj value for the individual's error on case and the population
   min error on case."
  [ind-error pop-min-error]
  (if (= ind-error pop-min-error) 1 0))

(defn calculate-hj-vector
  "Calculates the hj values for this individual."
  [individual min-error-per-case option]
  (let [ind-elitist-vector (map calculate-elitist-on-case
                                (:errors individual)
                                min-error-per-case)]
    (case option
      1 (let [;; E_yi is the number of cases on which this individual is elite
              E_yi (apply + ind-elitist-vector)]
          (replace {1 E_yi} ind-elitist-vector))
      2 ind-elitist-vector
      (throw (Exception. (str "Unrecognized plexicase option: " option))))))

(defn unnormalized-probability-distribution
  "hj function from plexicase paper across whole population
   Calculate probability distribution for individuals across all test cases

   Options: 
   individuals: a vector of individuals 
   num-cases: the number of test cases

   Returns: 
   A list of lists of unnormalized probabilities for each individual"
  [{:keys [individuals num-cases option] :or {option 1}}]
  (let [min-error-per-case (map (fn [c] (reduce min (map #(nth (:errors %) c)
                                                         individuals)))
                                (range num-cases))]
    (map #(calculate-hj-vector % min-error-per-case option)
         individuals)))
     
(defn normalize-probability-distribution
  "Calculates the Pj(yi) values for every case and every individual."
  [unnormalized-prob-distribution]
  (let [row-sums (doall (map #(reduce + %) (apply map list unnormalized-prob-distribution)))]
    (doall (map (fn [hj-vector]
                  (doall (map (fn nested-normalize-prob-dist-anon [hj row-sum] (float (/ hj row-sum)))
                              hj-vector
                              row-sums)))
                unnormalized-prob-distribution))))

(defn probability-distribution
  "Calculates P(yi) = the final probability for each individual."
  [normalized-prob-dist]
  (mapv tools/mean (doall normalized-prob-dist)))

(defn plexicase-select-all-parents
  "Selects all parents for the generation. This is done here instead of one
   parent at a time, because simple/sample is much faster that way."
  [individuals probability-distribution number-parents]
  (take number-parents
        (map #(nth individuals %)
             (simple/sample (range (count individuals))
                            :weigh (fn [indi] (nth probability-distribution indi))
                            :replace true))))

(defn make-plexicase-selection
  "Calculates the selection probabilities for every individual in the population.
   Params:
     state - a map that contains at least :individuals as a key. Can optionally
             take option, which is a number indicating which version of plexicase
             to use:
               option 1: original plexicase
               option 2: uses 1 instead of E(yi) in the calculation of hj"
  [num-parents {:keys [individuals option] :or {option 1}}]
  (let [num-cases (count (:errors (first individuals)))
        unnormalized-prob-dist (doall (unnormalized-probability-distribution {:individuals individuals :num-cases num-cases :option option}))
        normalized-prob-dist (doall (normalize-probability-distribution unnormalized-prob-dist))
        ind-probabilities (doall (probability-distribution normalized-prob-dist))]
    {:plexicase-parents (plexicase-select-all-parents individuals ind-probabilities num-parents)}))

(defn plexicase-select-parent-using-index
  "Uses plexicase to select a parent. Uses the parents already selected by
   plexicase-select-all-parents"
  [plexicase-parents index number-parents]
  (take number-parents
        (drop (* index number-parents)
              plexicase-parents)))

;; (defn select-parents-from-distribution
;;   "Uses plexicase to select a parent."
;;   [individuals probability-distribution number-parents]
;;   (map #(nth individuals %)
;;        (take number-parents (simple/sample (range (count individuals))
;;                               :weigh (fn [indi] (nth probability-distribution indi))
;;                               :replace true))))

(def test-population
  '({:errors (10 5 5 15 10)}
    {:errors (8 7 8 8 7)}
    {:errors (73 60 0 0 1)}
    {:errors (15 12 14 15 1)}
    {:errors (15 12 0 106 1)}))

(defn larger-test-population
  [pop-size num-cases]
  
  (repeatedly pop-size (fn [] 
                         {:errors (repeatedly num-cases #(rand-int 2))})))

(defn -main
  [pop & args]
  (make-plexicase-selection (merge {:individuals pop :num-cases 5}
                                   (apply hash-map args))))


(comment

  (-main test-population :option 2)

  (take 5 (simple/sample [:heads :tails]
                         :weigh {:heads 0.5 :tails 0.5}
                         :replace true))

  (def test-probability-distribution (-main test-population :option 2))
  (print test-probability-distribution)
  (def indices [1 2 3 4 5])

  (take 2 (simple/sample indices
                         :weigh (fn [indi] (nth test-probability-distribution (- indi 1)))
                        ;;  :replace true
                         ))
  )  
