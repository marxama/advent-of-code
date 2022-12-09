(ns advent-of-code.y2022.day09
  (:require [advent-of-code.util :as util]))

(defn parse-input-line [line]
  (let [[dir steps] (.split line " ")]
    {:steps (Integer/parseInt steps)
     :dir (case dir
            "R" [1 0]
            "L" [-1 0]
            "U" [0 1]
            "D" [0 -1])}))

(defn read-input [f]
  (->> f
       util/read-resource-lines
       (map parse-input-line)))

(defn adjacent? [[ax ay] [bx by]]
  (and (<= -1 (- ax bx) 1)
       (<= -1 (- ay by) 1)))

(defn clamp-magnitude-to-1 [x]
  (cond
    (< x -1) -1
    (> x 1) 1
    :else x))

(defn get-tail-move-dir [[head-x head-y] [tail-x tail-y]]
  [(clamp-magnitude-to-1 (- head-x tail-x))
   (clamp-magnitude-to-1 (- head-y tail-y))])

(defn move-tail [head tail]
  (if (adjacent? head tail)
    tail
    (util/vec+ tail (get-tail-move-dir head tail))))

(defn walk [{:keys [head tail tail-visited]} dir]
  (let [new-head (util/vec+ head dir)
        new-tail (move-tail new-head tail)]
    {:head new-head
     :tail new-tail
     :tail-visited (conj tail-visited new-tail)}))

(defn walk-all [steps]
  (->> steps
       (reduce (fn [state {:keys [dir steps]}]
                 (->> (iterate #(walk % dir) state)
                      (take (inc steps))
                      last))
               {:head [0 0] :tail [0 0] :tail-visited #{[0 0]}})))

(defn day09_1 []
  (-> "day09_1" read-input walk-all :tail-visited count))
