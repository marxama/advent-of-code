(ns advent-of-code.y2022.day07
  (:require [advent-of-code.util :as util]))

(defn process-line [{:keys [current-dir-path] :as filesystem} line]
  (cond
    (= :cd-up (:command line)) (update filesystem :current-dir-path #(-> % pop pop))
    (= :cd (:command line)) (let [filesystem (update filesystem :current-dir-path conj :dirs (:dir line))]
                              (assoc-in filesystem (:current-dir-path filesystem) {:files {} :dirs {} :name (:dir line)}))
    (= :file (:type line)) (assoc-in filesystem (conj current-dir-path :files (:name line)) {:size (:size line) :name (:name line)})
    :else filesystem))

(defn calculate-dir-sizes [{:keys [dirs files] :as dir}]
  (let [dir (->> (reduce (fn [dir subdir] (update-in dir [:dirs subdir] calculate-dir-sizes)) dir (keys dirs)))
        subdirs-total-size (->> dir :dirs vals (map :total-size) (apply +))
        current-dir-total-file-size (->> files vals (map :size) (apply +))]
    (assoc dir :total-size (+ subdirs-total-size current-dir-total-file-size))))

(defn build-filesystem [lines] 
  (as-> lines x
    (reduce process-line {:dirs {} :current-dir-path []} x)
    (select-keys x [:dirs])
    (calculate-dir-sizes x)))

(defn parse-line [line]
  (cond
    (.startsWith line "$ cd ..") {:command :cd-up}
    (.startsWith line "$ cd") {:command :cd :dir (.substring line 5)}
    (.startsWith line "$ ls") {:command :ls}
    (.startsWith line "dir ") {:type :dir :name (.substring line 4)}
    :else (let [[size name] (.split line " ")]
            {:type :file :name name :size (Long/parseLong size)})))

(defn read-input [f]
  (->> f
       util/read-resource-lines
       (map parse-line)
       build-filesystem))

(defn get-dirs [{:keys [dirs] :as dir}]
  (concat
   (->> [(dissoc dir :dirs :files)]
        (remove (fn [{:keys [name]}]
                  (or (nil? name) (= name "/")))))
   (mapcat #(get-dirs %) (vals dirs))))

(defn sum-dirs-with-total-size-not-larger-than [filesystem max-size]
  (->> filesystem
       get-dirs
       (filter (fn [{:keys [total-size]}] (<= total-size max-size)))
       (map :total-size)
       (apply +)))

(defn day07_1 []
  (sum-dirs-with-total-size-not-larger-than (read-input "y2022/day07_1") 100000))


(defn find-smallest-dir-not-smaller-than [filesystem min-size]
  (->> filesystem
       get-dirs
       (filter (fn [{:keys [total-size]}] (<= min-size total-size)))
       (sort-by :total-size)
       first))

(defn find-smallest-dir-to-free-up-space [filesystem total-disk-space disk-space-required]
  (let [available-space (- total-disk-space (:total-size filesystem))
        missing-disk-space (- disk-space-required available-space)]
    (:total-size (find-smallest-dir-not-smaller-than filesystem missing-disk-space))))

(defn day07_2 []
  (find-smallest-dir-to-free-up-space (read-input "y2022/day07_1") 70000000 30000000))
