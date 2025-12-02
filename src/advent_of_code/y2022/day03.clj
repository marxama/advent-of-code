(ns advent-of-code.y2022.day03
  (:require [advent-of-code.util :as util]
            [clojure.set :as s]))

(defn structure-compartment-items [compartment-items]
  {:items compartment-items
   :item-types (set compartment-items)})

(defn parse-rucksack [line]
  (let [items (vec (seq line))
        compartment-items (util/split-at-middle items)]
    (map structure-compartment-items compartment-items)))

(defn read-rucksacks-file [f]
  (->> f
       util/read-resource-lines
       (map parse-rucksack)))

(defn add-to-char [c x]
  (char (+ x (int c))))

(def priority-by-item-type
  (merge
   (->> (range 26)
        (map #(do [(add-to-char \a %) (+ % 1)]))
        (into {}))
   (->> (range 26)
        (map #(do [(add-to-char \A %) (+ % 27)]))
        (into {}))))

(defn find-common-item-types [rucksack]
  (->> rucksack
       (map :item-types)
       (reduce s/intersection)))

(defn sum-of-priorities-of-common-item-types [rucksacks]
  (->> rucksacks
       (mapcat find-common-item-types)
       (map priority-by-item-type)
       (reduce +)))

(defn sum-of-priorities-of-common-item-types-for-file [f]
  (->> f read-rucksacks-file sum-of-priorities-of-common-item-types))

(defn day03_1 []
  (sum-of-priorities-of-common-item-types-for-file "y2022/day03_1"))


(defn partition-groups [rucksacks]
  (partition 3 rucksacks))

(defn determine-group-badge [group-rucksacks]
  (->> group-rucksacks
       (map (fn [rucksack-compartments]
              (->> rucksack-compartments
                   (map :item-types)
                   (apply s/union))))
       (apply s/intersection)
       first))

(defn determine-groups [rucksacks]
  (->> rucksacks
       partition-groups
       (map (fn [group-rucksacks]
              {:group-badge (determine-group-badge group-rucksacks)
               :rucksacks group-rucksacks}))))

(defn sum-of-priorities-of-group-badges [rucksacks]
  (->> rucksacks
       determine-groups
       (map :group-badge)
       (map priority-by-item-type)
       (reduce +)))

(defn sum-of-priorities-of-group-badges-for-file [f]
  (->> f read-rucksacks-file sum-of-priorities-of-group-badges))

(defn day03_2 []
  (sum-of-priorities-of-group-badges-for-file "y2022/day03_1"))
