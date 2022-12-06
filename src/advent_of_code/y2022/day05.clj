(ns advent-of-code.y2022.day05
  (:require [advent-of-code.util :as util]
            [clojure.string :as string]))

(defn parse-move-line [line]
  (let [[_ count _ from _ to] (.split line " ")]
    {:crate-count (Integer/parseInt count)
     :from (Integer/parseInt from)
     :to (Integer/parseInt to)}))

(defn get-line-crate [line index]
  (let [char-index (+ 1 (* 4 index))
        crate (if (< char-index (count line))
                (str (.charAt line char-index)))]
    (if-not (= " " crate)
      crate)))

(defn parse-stacks [stack-lines]
  ;; Assumes that crates are single letters, and that horizontal distance between them is constant
  (let [stack-lines (butlast stack-lines)
        longest-line (apply max-key (partial count) stack-lines)
        stacks-count (/ (inc (count longest-line)) 4)]
    (->> (range stacks-count)
         (map (fn [index]
                (->> stack-lines
                     (map #(get-line-crate % index))
                     (remove nil?))))
         (zipmap (map inc (range stacks-count))))))

(defn read-input [f]
  (let [[stacks-lines _ moves-lines] (->> f util/read-resource-lines (partition-by string/blank?))]
    {:stacks (parse-stacks stacks-lines)
     :moves (map parse-move-line moves-lines)}))

(defn operate-crane [stacks {:keys [crate-count from to]} keep-order?]
  (let [items-to-move (cond-> (->> (get stacks from) (take crate-count) vec)
                        (not keep-order?) reverse)]
    (-> stacks
        (update from #(drop crate-count %))
        (update to #(concat items-to-move %)))))

(defn get-top-crates-str [stacks]
  (->> stacks
       (sort-by key)
       (map second)
       (map first)
       (remove nil?)
       (apply str)))

(defn read-file-and-apply-moves [f keep-order?]
  (let [{:keys [stacks moves]} (read-input f)] 
    (get-top-crates-str (reduce #(operate-crane %1 %2 keep-order?) stacks moves))))

(defn day05_1 []
  (read-file-and-apply-moves "day05_1" false))

(defn day05_2 []
  (read-file-and-apply-moves "day05_1" true))
