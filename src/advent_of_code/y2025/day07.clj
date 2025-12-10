(ns advent-of-code.y2025.day07
  (:require [advent-of-code.util :as util]))

(defn print-state [{:keys [grid]}]
  (doseq [line grid]
    (println (apply str line))))

(defn parse-input []
  (->> (util/read-resource-lines "y2025/day07")
       (mapv vec)))

; for the inputs we've got, we don't need to check the bounds
(defn split [x]
  [(dec x) (inc x)])

(defn step [{:keys [height current-row paths-per-x] :as state}]
  (if (= current-row (dec height))
    (assoc state :finished? true)
    (let [next-row (inc current-row)
          new-state (reduce (fn [{:keys [grid] :as state} [x path-count-for-x]]
                              (if (= \^ (get-in grid [next-row x]))
                                (let [[x1 x2] (split x)]
                                  (-> state
                                      (update :split-count inc)
                                      (update :paths-per-x (partial merge-with +) {x1 path-count-for-x x2 path-count-for-x})
                                      (update :paths-per-x dissoc x)))
                                state))
                            state
                            paths-per-x)]
      (reduce (fn [state x] (update state :grid assoc-in [next-row x] \|))
              (assoc new-state :current-row next-row)
              (keys (:paths-per-x new-state))))))

(defn initial-state []
  (let [grid (parse-input)
        start-x (.indexOf (first grid) \S)]
    {:grid grid
     :height (count grid)
     :current-row 0
     :paths-per-x {start-x 1}
     :split-count 0
     :finished? false}))

(defn day07_1 []
  (->> (initial-state)
       (iterate step)
       (drop-while (complement :finished?))
       first
       :split-count))

(defn day07_2 []
  ; the straight-forward solution - to calculate all paths - is too slow.
  ; the main thing to realize is that each time we split, the we get one more timeline.
  ; however, if two paths meet again, we need to "remember" that they belong to two
  ; separate paths/timelines. therefore, :paths-per-x keeps track of this over time,
  ; and in the end we just need to count the total number of paths across all x positions.
  (->> (initial-state)
       (iterate step)
       (drop-while (complement :finished?))
       first
       :paths-per-x
       vals
       (apply +)))
