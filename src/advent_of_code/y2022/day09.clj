(ns advent-of-code.y2022.day09
  (:require [advent-of-code.util :as util]))

(defn parse-input-line [line]
  (let [[dir steps] (.split line " ")
        steps (Integer/parseInt steps)
        dir (case dir
              "R" [1 0]
              "L" [-1 0]
              "U" [0 1]
              "D" [0 -1])]
    (repeat steps dir)))

(defn read-input [f]
  (->> f
       util/read-resource-lines
       (mapcat parse-input-line)))

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

(defn follow [head tail]
  (if (adjacent? head tail)
    tail
    (util/vec+ tail (get-tail-move-dir head tail))))

(defn walk [[head & remaining-rope] dir]
  (reduce (fn [result rope-piece]
            (conj result (follow (last result) rope-piece)))
          [(util/vec+ head dir)]
          remaining-rope))

(defn init-rope [length]
  (repeat length [0 0]))

(defn walk-all [rope steps]
  (reductions walk rope steps))

(defn count-tail-visited [f rope-length]
  (->> f
       read-input
       (walk-all (init-rope rope-length))
       (map last)
       distinct
       count))

(defn day09_1 []
  (count-tail-visited "day09_example_1" 2))

(defn day09_2 []
  (count-tail-visited "day09" 10))
