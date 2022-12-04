(ns advent.day2
  (:require [advent.io :refer [load-input]]
            [clojure.set :as set]))

;; https://adventofcode.com/2022/day/2

(defonce test-input "
A Y
B X
C Z")

(defn ->hand-p1 [c]
  (case c
    ("A" "X") :rock
    ("B" "Y") :paper
    :scissors))

(def hand-index {:rock 0
                 :paper 1
                 :scissors 2})

(def index-hand (set/map-invert hand-index))

(defn ->verus [h c]
  (index-hand (mod (+ (hand-index h) 
                      (case c
                        "X" -1
                        "Y" 0
                        "Z" 1)) 3)))

(defn compete [a b]
  (* 3
     (case (- (hand-index a) (hand-index b))
       0      1
       (1 -2) 0
       (-1 2) 2)))

(defn hand->score [h]
  (case h
    :rock 1
    :paper 2
    :scissors 3))

(defn score-p1 [[a b]]
  (let [h1 (->hand-p1 a)
        h2 (->hand-p1 b)]
    (+
     (compete h1 h2)
     (hand->score h2))))

(defn score-p2 [[a b]]
  (let [h1 (->hand-p1 a)
        h2 (->verus h1 b)]
    (+
     (compete h1 h2)
     (hand->score h2))))

(defn solve-p1 [input]
  (->> (.split input "\n")
       (filter (complement empty?))
       (map #(.split % " "))
       (map js->clj)
       (map score-p1)
       (reduce +)))

(defn solve-p2 [input]
  (->> (.split input "\n")
       (filter (complement empty?))
       (map #(.split % " "))
       (map js->clj)
       (map score-p2)
       (reduce +)))

(comment
  (solve-p1 test-input)
  (solve-p1 (load-input 2))
  (solve-p2 test-input)
  (solve-p2 (load-input 2)))