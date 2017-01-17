(ns alphabet-cipher.coder)

(def alphabet "abcdefghijklmnopqrstuvwxyz")

(defn rotate [coll n]
  (take (count coll) (drop n (cycle coll))))

(def rows-cols (for [i alphabet
                     j alphabet]
                 [i j]))

(def row-col-vals (mapcat (partial rotate alphabet) (range (count alphabet))))

(def encode-chart (into {} (map vector rows-cols row-col-vals)))

(def decode-chart (into {} (map (fn [[[row col] val]]
                                  [[row val] col])
                                encode-chart)))

(defn apply-transform [f message' message]
  (apply str (map (comp f vector) message' message)))

(defn encode [keyword message]
  (apply-transform encode-chart (cycle keyword) message))

(defn decode [keyword message]
  (apply-transform decode-chart (cycle keyword) message))

(defn find-repeat [coll]
  (loop [cnt 1]
    (let [partitions (partition cnt coll)]
      (cond (<= (count partitions) 1) coll
            (apply = partitions) (first partitions)
            :else (recur (inc cnt))))))

(defn decipher [cipher message]
  (apply str (find-repeat (apply-transform decode-chart message cipher))))
