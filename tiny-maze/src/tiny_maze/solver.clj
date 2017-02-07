(ns tiny-maze.solver)

(defn neighbors [maze coord]
  "Given a coordinate, returns the coordinates
  of the 'visitable' neighbors."
  (let [up-down-left-right [[1 0] [-1 0] [0 1] [0 -1]]]
    (->> (map #(mapv + % coord) up-down-left-right)
         (filter #(some? (get-in maze %)))
         (remove #(= (get-in maze %) 1)))))

(defn path [maze start end]
  "Returns the ordered coordinates to get from
  the start coord to the end coord using a bfs
  approach. Returns an empty vector, if no such
  path exists."
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
  "Returns the coordinate of x in the maze. If
  there is more than one, the first one is returned."
  (->> (for [r (range (count maze))
             c (range (count (first maze)))]
         [r c])
       (some #(and (= (get-in maze %) x) %))))

(defn assoc-in-maze [maze coords v]
  "Given a seq of coords and a maze, sets the
  coordinates to v."
  (reduce (fn [maze' coord]
            (assoc-in maze' coord v))
          maze
          coords))

(defn solve-maze [maze]
  "Finds the path from the start to the
  finish and highlights the path with :x"
  (let [start (find-in maze :S)
        end (find-in maze :E)
        path (path maze start end)]
    (assoc-in-maze maze path :x)))
