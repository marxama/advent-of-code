(ns advent-of-code.y2025.day03
  (:require [advent-of-code.util :as util]))

(defn max-n-digit-joltage [line n]
  (let [numbers (map #(Integer/parseInt (str %)) line)]
    (loop [numbers numbers
           n n
           result []]
      (if (zero? n)
        (Long/parseLong (apply str result)) ; this is so lazy ;D
        (let [max-num (apply max (take (- (count numbers) (dec n)) numbers))]
          (recur
           (drop 1 (drop-while #(not= % max-num) numbers))
           (dec n)
           (conj result max-num)))))))

(defn max-2-digit-joltage [line]
  (let [numbers (map #(Integer/parseInt (str %)) line)
        max-first (apply max (drop-last numbers)) 
        max-second (apply max (drop 1 (drop-while #(not= % max-first) numbers)))]
    (+ (* 10 max-first) max-second)))

(defn parse-input []
  (util/read-resource-lines "y2025/day03"))

(defn day03_1 []
  (->> (parse-input)
       (map max-2-digit-joltage)
       (reduce +)))

(defn day03_2 []
  (->> (parse-input)
       (map #(max-n-digit-joltage % 12))
       (reduce +)))
