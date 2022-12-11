(ns advent-of-code.y2022.day11
  (:require [advent-of-code.util :as util]
            [clojure.string :as string]))

(defn parse-monkey [[_ starting-items operation test if-true if-false]]
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
   :test (let [divisible-by (-> test (.split "divisible by ") last Long/parseLong)]
           #(zero? (rem % divisible-by)))
   :when-true (-> if-true (.split "throw to monkey ") last Long/parseLong)
   :when-false (-> if-false (.split "throw to monkey ") last Long/parseLong)
   :items-inspected 0})

(defn read-input [f]
  (->> f
       (util/read-resource-lines)
       (partition-all 7)
       (mapv parse-monkey)))

(defn inspect-once [current-monkey-index monkeys]
  (let [{:keys [items operation test when-true when-false]} (nth monkeys current-monkey-index)
        [item] items
        new-worry-level (operation item)
        new-worry-level (long (/ new-worry-level 3))
        throw-to (if (test new-worry-level)
                   when-true
                   when-false)]
    (-> monkeys
        (update-in [current-monkey-index :items] #(-> % rest vec))
        (update-in [current-monkey-index :items-inspected] + 1)
        (update-in [throw-to :items] conj new-worry-level))))

(defn inspect-all-monkeys-items [current-monkey-index monkeys]
  (reduce (fn [monkeys _] (inspect-once current-monkey-index monkeys)) monkeys (-> monkeys (nth current-monkey-index) :items)))

(defn play-round [monkeys]
  (reduce (fn [monkeys current-monkey-index]
            (inspect-all-monkeys-items current-monkey-index monkeys))
          monkeys
          (range (count monkeys))))

(defn print-monkeys [monkeys]
  (doseq [[index {:keys [items]}] (map-indexed vector monkeys)]
    (println (str "Monkey " index ": " (string/join ", " items)))))

(defn count-inspections-per-monkey [monkeys rounds]
  (->> (reductions (fn [monkeys round] (play-round monkeys)) monkeys (range))
       (take (inc rounds))
       last
       (map :items-inspected)
       sort
       reverse
       (take 2)
       (apply *)))

(defn day11_1 []
  (count-inspections-per-monkey (read-input "day11") 20))
