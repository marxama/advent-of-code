(ns advent-of-code.y2025.day01
  (:require [advent-of-code.util :as util]))

(defn parse-rotation [rotation]
  (let [sign (if (= (first rotation) \L) -1 1)
        steps (Integer/parseInt (subs rotation 1))]
    (* sign steps)))

(defn turn [current rotation]
  (-> rotation
      parse-rotation
      (+ current)
      (mod 100)))

(defn turn-all [initial rotations]
  (reductions turn initial rotations))

(defn calculate-password [rotations]
  (->> rotations
       (turn-all 0)))

(defn day01_1 []
  (-> "y2025/day01_example"
      util/read-resource-lines
      calculate-password))
