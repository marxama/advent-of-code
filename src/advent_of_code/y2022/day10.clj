(ns advent-of-code.y2022.day10
  (:require [advent-of-code.util :as util]))

(defn parse-line [line]
  (if (= "noop" line)
    [identity]
    (let [[_ amount] (.split line " ")]
      ;; Flattens "addx" to two operations, where the add operation now takes one cycle
      [identity (partial + (Integer/parseInt amount))])))

(defn read-input [f]
  (->> f
       util/read-resource-lines
       (mapcat parse-line)))

(defn execute-cpu [program]
  (vec (reductions #(%2 %1) 1 program)))

(defn signal-strength-during [states cycle]
  (* (inc cycle) ; account for 0-index
     (nth states cycle)))

(defn get-signal-strengths [states]
  (->> (range)
       (map #(+ 20 (* % 40)))
       (map dec) ; zero-indexed
       (take-while #(< % (count states)))
       (map #(signal-strength-during states %))))

(defn day10_1 []
  (->> "day10"
       read-input
       execute-cpu
       get-signal-strengths
       (apply +)))

(defn execute-screen [cpu-cycles]
  (doseq [line (->> cpu-cycles
                    (map-indexed (fn [position x]
                                   (if (<= (dec x) (mod position 40) (inc x))
                                     "#"
                                     ".")))
                    (partition 40)
                    (map #(apply str %)))]
    (println line)))

(defn day10_2 []
  (->> "day10"
       read-input
       execute-cpu
       execute-screen))