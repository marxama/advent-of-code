(ns advent-of-code.y2025.day03
  (:require [advent-of-code.util :as util]))

(defn max-joltage [line]
  (let [numbers (map #(Integer/parseInt (str %)) line)
        max-first (apply max (drop-last numbers)) 
        max-second (apply max (drop 1 (drop-while #(not= % max-first) numbers)))]
    (+ (* 10 max-first) max-second)))

(defn total-max-joltage [lines]
  (reduce + (map max-joltage lines)))

(defn parse-input []
  (util/read-resource-lines "y2025/day03"))

(defn day03_1 []
  (->> (parse-input)
       total-max-joltage))