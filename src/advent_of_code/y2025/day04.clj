(ns advent-of-code.y2025.day04
  (:require [advent-of-code.util :as util]))

(defn is-paper-roll? [grid [x y]]
  (= (get-in grid [y x]) \@))

(defn get-adjacent-positions [[x y]]
  (for [dx [-1 0 1]
        dy [-1 0 1]
        :when (not (and (zero? dx) (zero? dy)))]
    [(+ x dx) (+ y dy)]))

(defn count-adjacent-paper-rolls [grid pos]
  (->> (get-adjacent-positions pos)
       (filter #(is-paper-roll? grid %))
       count))

(defn mark-accessible-paper-roll [grid [x y]]
  (assoc-in grid [y x] \x))

(defn mark-accessible-paper-rolls [grid]
  (let [height (count grid)
        width (count (first grid))
        accessible-positions (for [y (range height)
                                   x (range width)
                                   :when (and (is-paper-roll? grid [x y])
                                              (< (count-adjacent-paper-rolls grid [x y]) 4))]
                               [x y])]
    (reduce mark-accessible-paper-roll grid accessible-positions)))

(defn print-grid [grid]
  (doseq [line grid]
    (println (apply str line))))

(defn count-accessible-paper-rolls [grid]
  (->> grid
       (mapcat (fn [line] (filter #(= % \x) line)))
       count))

(defn read-input []
  (->> "y2025/day04"
       util/read-resource-lines
       (mapv vec)))

(defn day04_1 []
  (-> (read-input)
       (mark-accessible-paper-rolls)
       (count-accessible-paper-rolls)))
