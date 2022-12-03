(ns advent-of-code.util
  (:require [clojure.string :as string]))

(defn read-resource [f]
  (slurp (str "resources/" f)))

(defn read-resource-lines [f]
  (-> f read-resource string/split-lines))

(defn split-at-middle [vec]
  (split-at (int (/ (count vec) 2)) vec))
