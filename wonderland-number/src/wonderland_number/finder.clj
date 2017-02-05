(ns wonderland-number.finder)

(defn same-digits? [n m]
  (let [f (comp set str)]
    (= (f n) (f m))))

(defn wonderland-number? [n]
  (->> (map (partial * n) [2 3 4 5 6])
       (every? (partial same-digits? n))))

(defn wonderland-number []
  (->> (range 100000 (inc 999999))
       (some #(and (wonderland-number? %) %))))
