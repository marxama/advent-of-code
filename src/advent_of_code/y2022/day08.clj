(ns advent-of-code.y2022.day08
  (:require [advent-of-code.util :as util]))

(defn determine-size [trees]
  [(count (first trees))
   (count trees)])

(defn read-input [f]
  (let [trees (->> f
                   util/read-resource-lines
                   (mapv (fn [line]
                           (->> line
                                seq
                                (map str)
                                (mapv #(Integer/parseInt %))))))
        size (determine-size trees)]
    {:trees trees
     :size size}))

(defn get-tree-height [trees [x y]]
  (-> trees
      (nth y)
      (nth x)))

(defn in-range? [{[max-x max-y] :size} [x y]]
  (and (< -1 x max-x)
       (< -1 y max-y)))

(defn walk
  "Walks one step towards dir. If we end up outside of the trees grid, nil is returned."
  [grid pos dir]
  (let [pos (mapv + pos dir)]
    (if (in-range? grid pos)
      pos
      nil)))

(defn get-all-positions-in-path [grid from dir]
  (->> (iterate #(walk grid % dir) from)
       (take-while some?)))

(defn get-visible-trees-from-edge [{:keys [trees] :as grid} from dir]
  (->> (get-all-positions-in-path grid from dir)
       (reduce (fn [{:keys [max-tree-height visible-trees] :as res} tree-pos]
                 (let [tree-height (get-tree-height trees tree-pos)]
                   (if (< max-tree-height tree-height)
                     {:max-tree-height tree-height :visible-trees (conj visible-trees tree-pos)}
                     res)))
               {:max-tree-height -1 :visible-trees #{}})
       :visible-trees))

(defn get-edge-positions [{[max-x max-y] :size}]
  (distinct
   (concat
    (for [x (range max-x)
          y [0 (dec max-y)]]
      [x y])
    (for [x [0 (dec max-x)]
          y (range max-y)]
      [x y]))))

(defn determine-dir [{[max-x max-y] :size} [x y :as pos]]
  (cond
    (zero? x) [1 0]
    (= (dec max-x) x) [-1 0]
    (zero? y) [0 1]
    (= (dec max-y) y) [0 -1]
    :else (throw (Exception. (str "No dir determined for " pos)))))

(defn count-visible-trees [grid]
  (->> (for [pos (get-edge-positions grid)
             :let [dir (determine-dir grid pos)]]
         (get-visible-trees-from-edge grid pos dir))
       (apply concat)
       distinct
       count))

(defn day08_1 []
  (-> "day08_1" read-input count-visible-trees))

;; Courtesy of https://stackoverflow.com/a/30928487/845595
(defn take-while+
  [pred coll]
  (lazy-seq
   (when-let [[f & r] (seq coll)]
     (if (pred f)
       (cons f (take-while+ pred r))
       [f]))))

(defn get-scenic-score-for-tree [{:keys [trees] :as grid} from-tree]
  (let [start-tree-height (get-tree-height trees from-tree)]
    (->> (for [dir [[1 0] [-1 0] [0 1] [0 -1]]]
           (->> (get-all-positions-in-path grid from-tree dir)
                (drop 1)
                (take-while+ #(< (get-tree-height trees %) start-tree-height))))
         (map count)
         (apply *))))

(defn get-inner-trees [{[max-x max-y] :size}]
  (distinct
   (for [x (range 1 (dec max-x))
         y (range 1 (dec max-y))]
     [x y])))

(defn find-maximum-scenic-score [grid]
  (->> grid
       (get-inner-trees)
       (map (partial get-scenic-score-for-tree grid))
       (apply max)))

(defn day08_2 []
  (-> "day08_1" read-input find-maximum-scenic-score))
