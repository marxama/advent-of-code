(ns advent-of-code.y2025.day02
  (:require [advent-of-code.util :as util]))

(defn repeats? [s n]
  (let [repetitions (/ (count s) n)]
    (= s (apply str (repeat repetitions (subs s 0 n))))))

(defn invalid? [d]
  (let [s (str d)
        length (count s)
        half-length (quot length 2)]
    (and (even? length)
         (repeats? s half-length))
    ;; I misread it first as "check for any repeating pattern"
    ;; - that might come in part 2, so leaving it here for now...
    #_(->> (range 1 (inc half-length))
         (some #(repeats? s %)))))

(defn parse-input [f]
  (->> (.split (util/read-resource f) ",")
       (map #(.split % "-"))
       (map (fn [num-strs] (mapv #(Long/parseLong %) num-strs)))
       (map (fn [[start end]] (range start (inc end))))))

(defn get-invalid-product-ids [f]
  ;; Could easily prune items here. Performance is good enough for now, but might come back to this later.
  (->> f
       parse-input
       (apply concat)
       (filter invalid?)))

(defn day02_1 []
  (apply + (get-invalid-product-ids "y2025/day02")))
