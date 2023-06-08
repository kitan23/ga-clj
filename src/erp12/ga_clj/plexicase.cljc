(ns erp12.ga-clj.plexicase)


(defn calculate_elitist
  "
   Calculate the elitist number for an individual
   "

  [individuals y_i]
  (let [individual (nth individuals y_i)
        num-cases (count (:errors individual))]
    (count (filter
            (fn [case] (= (nth (:errors individual) case) (reduce min (map #(nth (:errors %) case) individuals))))
            (range num-cases)))))

(defn unnormalized_probability

  "
   Helper function to calculate probability for an individual for a test case

   Options:
   individuals: a vector of individuals
   y_i: the index of the individual to calculate the probability for
   j: the index of the case to calculate the probability for
   "

;;   [individuals y_i j]
  [{:keys [individuals y_i j option] :or {option 1}}]
;;   [individuals y_i j {:keys [option] :or {option 1}}]
  (let [individual (nth individuals y_i)
        error_j (nth (:errors individual) j)
        min-error (reduce min (map #(nth (:errors %) j) individuals))
        E_y_i (calculate_elitist individuals y_i)]
    (if (= error_j min-error)
      (if (= option 1)
        E_y_i
        1)
      0)))

(defn unnormalized_probability_distribution
  "
   Calculate probability distribution for individuals across all test cases

   Options: 
   individuals: a vector of individuals 
   num-cases: the number of test cases

   Returns: 
   A list of lists of unnormalized probabilities for each individual 
   "

  [{:keys [individuals num-cases option] :or {option 1}}]
  (map
   (fn [y_i] (map
              (fn [j] (unnormalized_probability {:individuals individuals 
                                                 :y_i y_i 
                                                 :j j 
                                                 :option option}))
              (range num-cases)))
   (range (count individuals))))

(defn cases_probability_distribution
  [unnormalized_prob_distribution num_cases]
  (let [column-sums (mapv #(reduce + %) (apply mapv vector unnormalized_prob_distribution))]
    (mapv
     (fn [row] (mapv
                (fn [col-value i] (/ col-value (nth column-sums i)))
                row
                (range num_cases)))
     unnormalized_prob_distribution)))

(defn probability_distribution
  
  [cases_probability_distribution num_cases]
  (map #(float(/ (reduce + %) num_cases)) cases_probability_distribution)

  )

(defn make_plexicase_selection
  [{:keys [individuals  option] :or {option 1}}]
  (println "HELLO" (count individuals))

  (let [num-cases (count (:errors (first individuals)))
        unnormalized_prob_dist (unnormalized_probability_distribution {:individuals individuals :num-cases num-cases :option option})
        cases_prob_dist (cases_probability_distribution unnormalized_prob_dist num-cases)]
    (println (probability_distribution cases_prob_dist num-cases))))


(def test-population
  '({:errors (10 5 5 15 10)}
    {:errors (8 7 8 8 7)}
    {:errors (73 60 0 0 1)}
    {:errors (15 12 14 15 1)}
    {:errors (15 12 0 106 1)}))

(defn -main
  [& _]
  (make_plexicase_selection {:individuals test-population :num-cases 5 :option 1})
  )


(comment
(-main)
  )
