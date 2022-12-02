(ns advent-of-code.y2022.day02
  (:require [advent-of-code.util :as util]
            [clojure.set :as s]))

(def translate-shape-1
  {"A" :rock
   "X" :rock
   "B" :paper
   "Y" :paper
   "C" :scissors
   "Z" :scissors})

(defn read-strategy-guide-1 [f]
  (->> f
       util/read-resource-lines
       (map #(.split % " "))
       (map (fn [[theirs ours]]
              {:our-shape (translate-shape-1 ours)
               :their-shape (translate-shape-1 theirs)}))))

(def beats
  {:rock :scissors
   :paper :rock
   :scissors :paper})

(def beaten-by
  (s/map-invert beats))

(defn win? [our-shape their-shape]
  (= (beats our-shape) their-shape))

(defn outcome [our-shape their-shape]
  (cond
    (win? our-shape their-shape) :win
    (win? their-shape our-shape) :lose
    :else :draw))

(def shape-score
  {:rock 1
   :paper 2
   :scissors 3})

(defn calculate-score-by-shapes [our-shape their-shape]
  (+ (shape-score our-shape)
     (case (outcome our-shape their-shape)
       :win 6
       :lose 0
       :draw 3)))

(defn calculate-score-by-outcome [their-shape outcome]
  (calculate-score-by-shapes
   (case outcome
     :win (beaten-by their-shape)
     :lose (beats their-shape)
     :draw their-shape)
   their-shape))

(defn apply-strategy-guide-1 [strategy-guide-file]
  (->> strategy-guide-file
       read-strategy-guide-1
       (map (fn [{:keys [our-shape their-shape]}]
              (calculate-score-by-shapes our-shape their-shape)))
       (reduce +)))

(defn day02_1 []
  (apply-strategy-guide-1 "day02_1"))



(def translate-shape-2
  {"A" :rock
   "X" :lose
   "B" :paper
   "Y" :draw
   "C" :scissors
   "Z" :win})

(defn read-strategy-guide-2 [f]
  (->> f
       util/read-resource-lines
       (map #(.split % " "))
       (map (fn [[theirs ours]]
              {:their-shape (translate-shape-2 theirs)
               :outcome (translate-shape-2 ours)}))))

(defn apply-strategy-guide-2 [strategy-guide-file]
  (->> strategy-guide-file
       read-strategy-guide-2
       (map (fn [{:keys [outcome their-shape]}]
              (calculate-score-by-outcome their-shape outcome)))
       (reduce +)))

(defn day02_2 []
  (apply-strategy-guide-2 "day02_1"))
