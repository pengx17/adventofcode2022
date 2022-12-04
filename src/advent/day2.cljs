(ns advent.day2
  (:require [clojure.core.match :refer [match]]
            [advent.io :refer [load-input]]))

;; https://adventofcode.com/2022/day/2

(defonce test-input "
A Y
B X
C Z")

(defn win-score [[a b]]
  (* 3
     (match [a b]
       ["A" "X"] 1
       ["B" "X"] 0
       ["C" "X"] 2

       ["A" "Y"] 2
       ["B" "Y"] 1
       ["C" "Y"] 0

       ["A" "Z"] 0
       ["B" "Z"] 2
       ["C" "Z"] 1)))

(defn score [[a b]]
  (+ 
   (win-score [a b])
   (match b
       "X" 1
       "Y" 2
       "Z" 3)))

(defn solve-p1 [input]
  (->> (.split input "\n")
       (filter (complement empty?))
       (map #(.split % " "))
       (map js->clj)
       (map score)
       (reduce +)))

(comment
  (solve-p1 test-input)
  (solve-p1 (load-input 2)))