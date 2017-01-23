(ns alphabet-cipher.coder)

(def alphabet "abcdefghijklmnopqrstuvwxyz")

(defn rotate [coll n]
  (take (count coll) (drop n (cycle coll))))

(def rows-cols (for [i alphabet
                     j alphabet]
                 [i j]))

(def row-col-vals (mapcat (partial rotate alphabet) (range (count alphabet))))

(def encode-chart (into (sorted-map) (map vector rows-cols row-col-vals)))

(def decode-chart (into (sorted-map) (map (fn [[[row col] val]]
                                            [[row val] col])
                                          encode-chart)))
(defn chart-fn [chart]
  (fn [c' c]
    (get chart [c' c] c)))

(defn transform [f message' message]
  (apply str (map f message' message)))

(defn encode [keyword message]
  (transform (chart-fn encode-chart) (cycle keyword) message))

(defn decode [keyword message]
  (transform (chart-fn decode-chart) (cycle keyword) message))

(defn find-repeat [coll]
  (loop [cnt 1]
    (let [partitions (partition cnt coll)]
      (cond (<= (count partitions) 1) coll
            (apply = partitions) (first partitions)
            :else (recur (inc cnt))))))

(defn decipher [cipher message]
  (apply str (find-repeat (transform (chart-fn decode-chart) message cipher))))

(comment

  (def bible-url "http://www.gutenberg.org/cache/epub/10/pg10.txt")
  (def bible-encoded-file (str (System/getProperty "user.home") "/bible_encoded.txt"))
  (spit bible-encoded-file (encode "scones" (slurp bible-url)))


  (decode "scones" (slurp bible-encoded-file)))