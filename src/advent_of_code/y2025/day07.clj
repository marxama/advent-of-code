(ns advent-of-code.y2025.day07
  (:require [advent-of-code.util :as util]))

(defn print-state [{:keys [grid]}]
  (doseq [line grid]
    (println (apply str line))))

(defn parse-input []
  (->> (util/read-resource-lines "y2025/day07")
       (mapv vec)))

(defn split [{:keys [width]} x]
  (->> [(dec x) (inc x)]
       (filter #(<= 0 % (dec width)))))

(defn step [{:keys [height current-row current-xs] :as state}]
  (if (= current-row (dec height))
    (assoc state :finished? true)
    (let [next-row (inc current-row)
          new-state (reduce (fn [{:keys [grid] :as state} x] 
                              (if (= \^ (get-in grid [next-row x]))
                                (let [new-xs (split state x)] 
                                  (-> state
                                      (update :split-count inc)
                                      (update :current-xs into new-xs)))
                                (update state :current-xs conj x)))
                            (assoc state :current-xs #{})
                            current-xs)]
      (reduce (fn [state x] (update state :grid assoc-in [next-row x] \|))
              (assoc new-state :current-row next-row)
              (:current-xs new-state)))))

(defn initial-state []
  (let [grid (parse-input)
        start-x (.indexOf (first grid) \S)]
    {:grid grid
     :height (count grid)
     :width (count (first grid))
     :current-row 0
     :current-xs #{start-x}
     :split-count 0
     :finished? false}))

(defn day07_1 []
  (->> (initial-state)
       (iterate step)
       (drop-while (complement :finished?))
       first
       :split-count))
