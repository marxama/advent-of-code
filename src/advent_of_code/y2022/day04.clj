(ns advent-of-code.y2022.day04
  (:require [advent-of-code.util :as util]))

(defn parse-range [s]
  (->> (.split s "-")
       (map #(Integer/parseInt %))
       vec))

(defn parse-pair [pair]
  (->> (.split pair ",")
       (map parse-range)
       vec))

(defn read-input [f]
  (->> f
       util/read-resource-lines
       (map parse-pair)))

(defn fully-overlapped-by? [[is-overlapped-from is-overlapped-to] [is-overlapped-by-from is-overlapped-by-to]]
  (<= is-overlapped-by-from is-overlapped-from is-overlapped-to is-overlapped-by-to))

(defn one-is-fully-overlapped? [[a b]]
  (or (fully-overlapped-by? a b)
      (fully-overlapped-by? b a)))

(defn count-fully-overlapping-pairs [f]
  (->> f
       read-input
       (filter one-is-fully-overlapped?)
       count))

(defn day04_1 []
  (count-fully-overlapping-pairs "day04_1"))

(defn overlap? [[[a-from a-to :as a] [b-from b-to :as b]]]
  (->> [[a-from b]
        [a-to b]
        [b-from a]
        [b-to a]]
       (filter (fn [[x [from to]]]
                 (<= from x to)))
       seq))

(defn count-overlapping-pairs [f]
  (->> f
       read-input
       (filter overlap?)
       count))

(defn day04_2 []
  (count-overlapping-pairs "day04_1"))
