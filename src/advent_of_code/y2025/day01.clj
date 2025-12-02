(ns advent-of-code.y2025.day01
  (:require [advent-of-code.util :as util]))

(defn parse-rotation [rotation]
  (let [sign (if (= (first rotation) \L) -1 1)
        steps (Integer/parseInt (subs rotation 1))]
    (* sign steps)))

(defn turn [position rotation]
  (-> rotation
      parse-rotation
      (+ position)))

(defn turn-all [initial rotations]
  (reductions (comp #(mod % 100) turn) initial rotations))

(defn calculate-password [rotations]
  (->> rotations
       (turn-all 50)
       (filter zero?)
       count))

(defn day01_1 []
  (-> "y2025/day01"
      util/read-resource-lines
      calculate-password))

(defn sign [x]
  (cond
    (pos? x) 1
    (neg? x) -1
    :else 0))

(defn turn-all-0x434C49434B [initial rotations]
  (reductions
   (fn [[position _] rotation]
     (let [new-position (turn position rotation)]
       [(mod new-position 100)
        (+ (Math/abs (int (/ new-position 100)))
           ; Below handles the case where we cross zero in the negative direction
           ; (e.g. from 1 to -1, or 1 to 0)
           (if (and (pos? position) (not (pos? new-position)))
             1
             0))]))
   [initial 0]
   rotations))

(defn calculate-password-0x434C49434B [rotations]
  (->> rotations
       (turn-all-0x434C49434B 50)
       (map second)
       (apply +)))

(defn day01_2 []
  (-> "y2025/day01_example"
      util/read-resource-lines
      calculate-password-0x434C49434B))
