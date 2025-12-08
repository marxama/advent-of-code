(ns advent-of-code.y2025.day06
  (:require [advent-of-code.util :as util]
            [clojure.string :as string]))

(defn parse-operator [operator]
  (case operator
    "*" *
    "+" +
    \* *
    \+ +))

(defn parse-input []
  (->> (util/read-resource-lines "y2025/day06")
       (map string/trim)
       (map #(string/split % #"\s+"))
       (apply map vector) ; transpose
       (map reverse)
       (map (fn [[operator & operands]]
              {:operator (parse-operator operator)
               :operands (map parse-long operands)}))))

(defn execute-operations [operations]
  (map (fn [{:keys [operator operands]}]
         (apply operator operands))
       operations))

(defn day06_1 []
  (->> (parse-input)
       execute-operations
       (apply +)))

(defn parse-input-part-2 []
  (->> (util/read-resource-lines "y2025/day06")
       (apply map str) ; transpose the characters rather than the numbers
       (map string/trim)
       (partition-by string/blank?)
       (take-nth 2)
       (map (fn [[operator-line :as lines]]
              (let [operator-char (last operator-line)
                    operator (parse-operator operator-char)
                    operands (->> lines
                                  (map #(re-find #"\d+" %))
                                  (map parse-long))]
                {:operator operator
                 :operands operands})))))


(defn day06_2 []
  (->> (parse-input-part-2)
       execute-operations
       (apply +)))
