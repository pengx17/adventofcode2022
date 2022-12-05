(ns advent.day3
  (:require [advent.io :refer [load-input]]
            [clojure.string :as string]))

;; https://adventofcode.com/2022/day/3

(def test-input "
vJrwpWtwJgWrhcsFMMfFFhFp
jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL
PmmdzqPrVvPwwTWBwg
wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn
ttgJtRGJQctTZtZT
CrZsJsPPZsGzwwsLwLmpwMDw
")

(defn split-row [row]
  [(subs row 0 (/ (count row) 2))
   (subs row (/ (count row) 2))])

(defn find-duplicate
  ([xs ys]
   (let [set-xs (set xs)]
     (set (filter set-xs ys))))
  ([xs ys zs]
   (find-duplicate (find-duplicate xs ys) zs)))

(defn ->score [c]
  (let [code (.charCodeAt c 0)
        [a z] [(.charCodeAt "a" 0) (.charCodeAt "z" 0)]
        A (.charCodeAt "A" 0)]
    (if (and (>= code a) (<= code z))
      (inc (- code a))
      (+ 27 (- code A)))))

(defn solve-p1 [input]
  (->> (string/split-lines input)
       (filter (complement empty?))
       (map string/trim)
       (map split-row)
       (map #(apply find-duplicate %))
       (reduce into [])
       (map ->score)
       (reduce +)))

(defn solve-p2 [input]
  (->> (string/split-lines input)
       (filter (complement empty?))
       (map string/trim)
       (partition 3)
       (map #(apply find-duplicate %))
       (map first)
       (map ->score)
       (reduce +)))

(comment
  (solve-p1 test-input)
  (solve-p1 (load-input 3))
  (solve-p2 (load-input 3)))