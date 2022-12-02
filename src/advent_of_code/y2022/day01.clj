(ns advent-of-code.y2022.day01
  (:require [clojure.string :as string]
            [advent-of-code.util :as util]))

(defn read-elf-food-calories [f]
  (->> f
       util/read-resource-lines
       (partition-by string/blank?)
       (take-nth 2)
       (map (fn [lines]
              (map #(Integer/parseInt %) lines)))))

(defn accumulate-food-calories-per-elf [elf-food-calories]
  (map (partial apply +) elf-food-calories))

(defn day01_1 []
  (->> "day01_1"
       read-elf-food-calories
       accumulate-food-calories-per-elf
       (apply max)))

(defn day01_2 []
  (->> "day01_1"
       read-elf-food-calories
       accumulate-food-calories-per-elf
       sort
       reverse
       (take 3)
       (apply +)))
