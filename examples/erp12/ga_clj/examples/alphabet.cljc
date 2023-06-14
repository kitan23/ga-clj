(ns erp12.ga-clj.examples.alphabet
  (:gen-class)
  (:require [erp12.ga-clj.search.ga :as ga]
            [erp12.ga-clj.toolbox :as tb]
            [erp12.ga-clj.plexicase :as plx]
            [clj-async-profiler.core :as prof]))

(def target
  #_(vec "ABCDEFGHIJKLMNOPQRSTUVWXYZ")
  (mapv char (map #(+ 32 %) (range 200))))

(def tournament
  (tb/make-tournament-selection {:by :error :size 7}))

(def lexicase-selection
  (tb/make-lexicase-selection {:epsilon :num-errors})
  )

(defn -main
  "Evolves vector of letters in alphabetical order."
  [& _]
  (println
   (ga/run {;; Generates random genomes as a permutation of the target genome.
            :genome-factory  #(shuffle target)
             ;; Individuals are a map containing a scalar `:error` for the genome.
             ;; In this case, we use the hamming distance.
             ;; The `:genome` is added implicitly.
            :evaluator       (fn [gn _]
                               {:error (tb/hamming-distance gn target)
                                :errors (map #(if (= %1 %2) 0 1) gn target)})

            :post-eval  plx/make-plexicase-selection

             ;; To "breed" a new genome from the population, we:
             ;;   1. Select 2 parents with tournament selection.
             ;;   2. Pass their genomes to uniform-crossover.
             ;;   3. Mutate the resulting genome by swapping the position of 2 genes.
            :breed           (fn [{:keys [individuals]}]
                                ;; (->> (repeatedly 2 #(tournament individuals))
                               (->> (repeatedly 2 #(lexicase-selection individuals {:context "Hello"}))

                                    (map :genome)
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
                                 (= (:error best) 0) :solution-found
                                 (= step 300) :max-step-reached))
             ;; Each generation will contain 1000 individuals.
            :population-size 1000}))
  (shutdown-agents))



(comment

  (prof/profile (-main))

  (prof/serve-ui 8080)

  )