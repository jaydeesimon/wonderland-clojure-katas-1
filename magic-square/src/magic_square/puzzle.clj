(ns magic-square.puzzle)

(def values [1.0 1.5 2.0
             2.5 3.0 3.5
             4.0 4.5 5.0])

(defn sum-group [values index-group]
  (reduce (fn [sum i]
            (+ sum (nth values i)))
          0
          index-group))

(defn magic-square? [values]
  (let [rows [[0 1 2] [3 4 5] [6 7 8]]
        cols [[0 3 6] [1 4 7] [2 5 8]]
        diag [[0 4 8] [2 4 6]]]
    (apply = (map (partial sum-group values) (concat rows cols diag)))))

(defn permutations [coll]
  (if (= (count coll) 1)
    (list coll)
    (for [head coll
          tail (permutations (disj (set coll) head))]
      (lazy-seq (cons head tail)))))

(defn magic-square [values]
  (->> (some #(and (magic-square? %) %) (permutations values))
       (partition 3)
       (mapv vec)))
