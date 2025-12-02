(ns advent-of-code.y2022.day02
  (:require [advent-of-code.util :as util]
            [clojure.set :as s]))

(def translate-their-shape
  {"A" :rock
   "B" :paper
   "C" :scissors})

(def translate-our-shape
  {"X" :rock
   "Y" :paper
   "Z" :scissors})

(defn read-strategy-guide-1 [f]
  (->> f
       util/read-resource-lines
       (map #(.split % " "))
       (map (fn [[theirs ours]]
              {:our-shape (translate-our-shape ours)
               :their-shape (translate-their-shape theirs)}))))

(def beats
  {:rock :scissors
   :paper :rock
   :scissors :paper})

(def beaten-by
  (s/map-invert beats))

(defn win? [our-shape their-shape]
  (= (beats our-shape) their-shape))

(defn determine-outcome [our-shape their-shape]
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
     (case (determine-outcome our-shape their-shape)
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
  (apply-strategy-guide-1 "y2022/day02_1"))



(def translate-outcome
  {"X" :lose
   "Y" :draw
   "Z" :win})

(defn read-strategy-guide-2 [f]
  (->> f
       util/read-resource-lines
       (map #(.split % " "))
       (map (fn [[theirs ours]]
              {:their-shape (translate-their-shape theirs)
               :outcome (translate-outcome ours)}))))

(defn apply-strategy-guide-2 [strategy-guide-file]
  (->> strategy-guide-file
       read-strategy-guide-2
       (map (fn [{:keys [outcome their-shape]}]
              (calculate-score-by-outcome their-shape outcome)))
       (reduce +)))

(defn day02_2 []
  (apply-strategy-guide-2 "y2022/day02_1"))
