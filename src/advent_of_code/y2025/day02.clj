(ns advent-of-code.y2025.day02
  (:require [advent-of-code.util :as util]))

(defn repeats? [s n]
  (let [repetitions (/ (count s) n)]
    (= s (apply str (repeat repetitions (subs s 0 n))))))

(defn invalid? [d part1?]
  (let [s (str d)
        length (count s)
        half-length (quot length 2)]
    (if part1?
      (and (even? length)
           (repeats? s half-length))
      (->> (range 1 (inc half-length))
           (some #(repeats? s %))))))

(defn parse-input [f]
  (->> (.split (util/read-resource f) ",")
       (map #(.split % "-"))
       (map (fn [num-strs] (mapv #(Long/parseLong %) num-strs)))
       (map (fn [[start end]] (range start (inc end))))))

(defn get-invalid-product-ids [f part1?]
  ;; Could easily prune items here. Performance is good enough for now, but might come back to this later.
  (->> f
       parse-input
       (apply concat)
       (filter #(invalid? % part1?))))

(defn day02_1 []
  (apply + (get-invalid-product-ids "y2025/day02" true)))

(defn day02_2 []
  (apply + (get-invalid-product-ids "y2025/day02" false)))
