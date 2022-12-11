(ns advent-of-code.y2022.day11
  (:require [advent-of-code.util :as util]
            [clojure.string :as string]))

(defn parse-monkey [[_ starting-items operation test if-true if-false]]
  (let [test-num (-> test (.split "divisible by ") last Long/parseLong)]
    {:items (-> starting-items (.split ": ") second (.split ", ") (->> (mapv #(Long/parseLong %))))
     :operation (let [[a op b] (-> operation (.split "new = ") last (.split " "))
                      op (case op
                           "*" *
                           "+" +)]
                  #(op (if (= a "old")
                         %
                         (Long/parseLong a))
                       (if (= b "old")
                         %
                         (Long/parseLong b))))
     :test #(zero? (rem % test-num))
     :test-num test-num
     :when-true (-> if-true (.split "throw to monkey ") last Integer/parseInt)
     :when-false (-> if-false (.split "throw to monkey ") last Integer/parseInt)
     :items-inspected 0}))

(defn read-input [f]
  (->> f
       (util/read-resource-lines)
       (partition-all 7)
       (mapv parse-monkey)))

(defn inspect-once [monkeys current-monkey-index divide-by-three? max-num]
  (let [{:keys [items operation test when-true when-false]} (nth monkeys current-monkey-index)
        [item] items
        new-worry-level (operation item)
        new-worry-level (if divide-by-three? (long (/ new-worry-level 3)) new-worry-level)
        new-worry-level (if (and max-num (<= max-num new-worry-level))
                          (mod new-worry-level max-num)
                          new-worry-level)
        throw-to (if (test new-worry-level)
                   when-true
                   when-false)]
    (-> monkeys
        (update-in [current-monkey-index :items] #(-> % rest vec))
        (update-in [current-monkey-index :items-inspected] + 1)
        (update-in [throw-to :items] conj new-worry-level))))

(defn inspect-all-monkeys-items [monkeys current-monkey-index divide-by-three? max-num]
  (reduce (fn [monkeys _] (inspect-once monkeys current-monkey-index divide-by-three? max-num)) monkeys (-> monkeys (nth current-monkey-index) :items)))

(defn play-round [monkeys divide-by-three? max-num]
  (reduce (fn [monkeys current-monkey-index]
            (inspect-all-monkeys-items monkeys current-monkey-index divide-by-three? max-num))
          monkeys
          (range (count monkeys))))

(defn print-monkeys [monkeys]
  (doseq [[index {:keys [items]}] (map-indexed vector monkeys)]
    (println (str "Monkey " index ": " (string/join ", " items)))))

(defn print-monkeys-items-inspected [monkeys]
  (doseq [[index {:keys [items-inspected]}] (map-indexed vector monkeys)]
    (println (str "Monkey " index ": " items-inspected))))

(defn count-inspections-per-monkey [monkeys rounds divide-by-three?]
  (let [max-num (->> monkeys (map :test-num) distinct (apply *))]
    (->> (reductions (fn [monkeys round] (play-round monkeys divide-by-three? max-num)) monkeys (range))
         (take (inc rounds))
         last
         (map :items-inspected)
         sort
         reverse
         (take 2)
         (apply *))))

(defn day11_1 []
  (count-inspections-per-monkey (read-input "day11") 20 true))

(defn day11_2 []
  (count-inspections-per-monkey (read-input "day11") 10000 false))
