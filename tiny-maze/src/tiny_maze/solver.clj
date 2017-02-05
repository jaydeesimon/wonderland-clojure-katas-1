(ns tiny-maze.solver)

(defn neighbors [maze coord]
  (let [deltas [[1 0] [-1 0] [0 1] [0 -1]]]
    (->> (map #(mapv + % coord) deltas)
         (filter #(some? (get-in maze %)))
         (remove #(= (get-in maze %) 1)))))

(defn path [maze start end]
  (loop [frontier [[start [start]]]
         visited #{start}]
    (let [[[current path] & frontier] frontier
          unvisited (->> (neighbors maze current)
                         (remove visited))]
      (cond (nil? current) []
            (= current end) path
            :else (recur (into (vec frontier)
                               (mapv #(vector % (conj path %)) unvisited))
                         (conj visited current))))))

(defn find-in [maze x]
  (->> (for [r (range (count maze))
             c (range (count (first maze)))]
         [r c])
       (some #(and (= (get-in maze %) x) %))))

(defn illustrate-path [maze path-coords k]
  (reduce (fn [maze' coord]
            (assoc-in maze' coord k))
          maze
          path-coords))

(defn solve-maze [maze]
  (let [start (find-in maze :S)
        end (find-in maze :E)
        path (path maze start end)]
    (illustrate-path maze path :x)))
