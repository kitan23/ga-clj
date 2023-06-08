(ns erp12.ga-clj.plexicase
  (:require [erp12.ga-clj.toolbox :as tools]))

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
  (let [row-sums (map #(reduce + %) (apply map list unnormalized-prob-distribution))]
    (map (fn [hj-vector]
           (map (fn [hj row-sum] (/ hj row-sum))
                hj-vector
                row-sums))
         unnormalized-prob-distribution)))

(defn probability-distribution
  "Calculates P(yi) = the final probability for each individual."
  [normalized-prob-dist]
  (map tools/mean normalized-prob-dist))

(defn make-plexicase-selection
  "Calculates the selection probabilities for every individual in the population.
   Params:
     state - a map that contains at least :individuals as a key. Can optionally
             take option, which is a number indicating which version of plexicase
             to use:
               option 1: original plexicase
               option 2: uses 1 instead of E(yi) in the calculation of hj"
  [{:keys [individuals option] :or {option 1}}]
  (let [start-time (System/currentTimeMillis)
        num-cases (count (:errors (first individuals)))
        unnormalized-prob-dist (unnormalized-probability-distribution {:individuals individuals :num-cases num-cases :option option})
        normalized-prob-dist (normalize-probability-distribution unnormalized-prob-dist)
        ind-probabilities (doall (probability-distribution normalized-prob-dist)) ;; TODO: Remove the doall after testing
        end-time (System/currentTimeMillis)]
    (println \newline "ms taken during plexicase:" (- end-time start-time) \newline)
    ;; (println "Individual probabilities:" ind-probabilities) ;; TODO: eventually replace this with whatever we need to do to return the state with the probabilities set 
    nil))


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

  (replace {1 6} [0 1 0 1 1 0 1])

  (larger-test-population 200 20)

  (time (-main (larger-test-population 1000 26) :option 2))
  )
