(ns advent-of-code.y2025.day06
  (:require [advent-of-code.util :as util]
            [clojure.string :as string]))

(defn parse-input []
  (->> (util/read-resource-lines "y2025/day06")
       (map string/trim)
       (map #(string/split % #"\s+"))
       (apply map vector)
       (map reverse)
       (map (fn [[operator & operands]]
              {:operator (case operator
                           "*" *
                           "+" +)
               :operands (map parse-long operands)}))))

(defn day06_1 []
  (->> (parse-input)
       (map (fn [{:keys [operator operands]}]
              (apply operator operands)))
       (apply +)))