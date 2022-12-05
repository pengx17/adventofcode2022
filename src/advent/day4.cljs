(ns advent.day4
  (:require [cljs.tools.reader :refer [read-string]]
            [clojure.string :as string]
            [advent.io :refer [load-input]]))

;; https://adventofcode.com/2022/day/4


(def test-input "
2-4,6-8
2-3,4-5
5-7,7-9
2-8,3-7
6-6,4-6
2-6,4-8")

(defn fully-overlap? [row]
  (let [[[a0 a1] [b0 b1]] (->> (string/split row ",")
                               (map #(string/split % "-"))
                               (map (partial map read-string)))]
    (or (and (<= a0 b0) (>= a1 b1))
        (and (<= b0 a0) (>= b1 a1)))))

(defn overlap? [row]
  (let [[[a0 a1] [b0 b1]] (->> (string/split row ",")
                               (map #(string/split % "-"))
                               (map (partial map read-string)))]
    (not (or (< a1 b0) (> a0 b1)))))

(comment
  (fully-overlap? "2-4,6-8")
  (fully-overlap? "2-8,3-7")
  (->> (string/split-lines test-input)
       (filter (complement empty?))
       (map overlap?)))

(defn solve-p1 [input]
  (->> (string/split-lines input)
       (filter (complement empty?))
       (filter fully-overlap?)
       (count)))

(defn solve-p2 [input]
  (->> (string/split-lines input)
       (filter (complement empty?))
       (filter overlap?)
       (count)))

(comment
  (solve-p1 test-input)
  (solve-p1 (load-input 4))
  (solve-p2 (load-input 4)))

