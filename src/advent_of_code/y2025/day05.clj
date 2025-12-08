(ns advent-of-code.y2025.day05
  (:require [advent-of-code.util :as util]
            [clojure.string :as string]))

(defn fresh? [fresh-ranges ingredient-id]
  (some (fn [{:keys [from to]}]
          (<= from ingredient-id to))
        fresh-ranges))

(defn parse-input []
  (let [lines (remove string/blank? (util/read-resource-lines "y2025/day05"))
        [ranges ingredient-ids] (partition-by #(.contains % "-") lines)]
    {:fresh-ranges (->> ranges
                        (map #(let [[from to] (string/split % #"-")]
                                {:from (parse-long from)
                                 :to (parse-long to)})))
     :ingredient-ids (map parse-long ingredient-ids)}))

(defn day05_1 []
  (let [{:keys [fresh-ranges ingredient-ids]} (parse-input)]
    (->> ingredient-ids
         (filter #(fresh? fresh-ranges %))
         count)))