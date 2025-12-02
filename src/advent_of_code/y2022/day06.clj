(ns advent-of-code.y2022.day06
  (:require [advent-of-code.util :as util]))

(defn start-of-marker-count [s char-count]
  (->> s
       seq
       (partition char-count 1) ; There are much more efficient ways of doing this (:
       (map-indexed vector)
       (filter #(-> % second set count (= char-count)))
       ffirst
       (+ char-count)))

(defn day06_1 []
  (-> "y2022/day06_1" util/read-resource (start-of-marker-count 4)))

(defn day06_2 []
  (-> "y2022/day06_1" util/read-resource (start-of-marker-count 14)))
