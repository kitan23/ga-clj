(ns erp12.ga-clj.examples.alphabet
  (:gen-class)
  (:require [erp12.ga-clj.search.ga :as ga]
            [erp12.ga-clj.toolbox :as tb]
            [erp12.ga-clj.plexicase :as plx]
            [clj-async-profiler.core :as prof]))

(def target
  (vec "ABCDEFGHIJKLMNOPQRSTUVWXYZ")
  #_(mapv char (map #(+ 32 %) (range 200))))

(def tournament
  (tb/make-tournament-selection {:by :error :size 7}))

(def lexicase-selection
  (tb/make-lexicase-selection {:epsilon :num-errors}))

(defn -main
  "Evolves vector of letters in alphabetical order."
  [& _]
  (println
   (let [pop-size 1000]
     (ga/run {;; Generates random genomes as a permutation of the target genome.
              :genome-factory  #(shuffle target)

             ;; Individuals are a map containing a scalar `:error` for the genome.
             ;; In this case, we use the hamming distance.
             ;; The `:genome` is added implicitly.
              :evaluator       (fn [gn _]
                                 (let [errors (mapv #(if (= %1 %2) 0 1) gn target)]
                                   {:error (apply + errors)
                                    :errors errors}))

              :post-eval  #(plx/make-plexicase-selection (* 2 pop-size) (assoc % :num-errors (count target)))

             ;; To "breed" a new genome from the population, we:
             ;;   1. Select 2 parents with tournament selection.
             ;;   2. Pass their genomes to uniform-crossover.
             ;;   3. Mutate the resulting genome by swapping the position of 2 genes.
              :breed           (fn 
                                ;;  [{:keys [plexicase-parents index ]}]
                                 [{:keys [individuals]}]
                                ;; (->> (repeatedly 2 #(tournament individuals))
                                 (->> (repeatedly 2 #(lexicase-selection individuals {:context "Hello"}))
                                ;;  (->> 
                                ;;        (vector (plx/plexicase-select-parent-using-index
                                ;;                 {:plexicase-parents plexicase-parents
                                ;;                  :index (* 2 index)})
                                ;;                (plx/plexicase-select-parent-using-index
                                ;;                 {:plexicase-parents plexicase-parents
                                ;;                  :index (inc(* 2 index))}))
                                      (mapv :genome)
                                      tb/uniform-crossover
                                      tb/swap-2-genes))

             ;; We compare individuals on the basis of the error values. Lower is better.
              :individual-cmp  (comparator #(< (:error %1) (:error %2)))

             ;; We stop evolution when either:
             ;;   1. We find an individual with zero error or
             ;;   2. We reach 300 generations. 
              :stop-fn         (fn [{:keys [step best]}]
                                 (println "Step:" step "\tBest:" best)
                                 (cond
                                   (<= (:error best) 0) :solution-found
                                   (= step 300) :max-step-reached))

             ;; Each generation will contain 1000 individuals.
              :population-size pop-size

              ;:mapper map
              })))
  (shutdown-agents))



(comment
  (do
    (prof/profile (time (-main)))
    (prof/serve-ui 8080))

  )