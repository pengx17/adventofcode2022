(ns advent.day1
  (:require [clojure.string :as string]
            [advent.io :refer [load-input]]))

;; https://adventofcode.com/2022/day/1

(defn solve-p1 [input]
  (->> input
       (string/split-lines)
       (partition-by empty?)
       (filter #(not (every? empty? %)))
       (map (fn [xs] (->> xs
                          (map js/parseInt)
                          (reduce +))))
       (apply max)))

(defn solve-p2 [input]
  (->> input
       (string/split-lines)
       (partition-by empty?)
       (filter #(not (every? empty? %)))
       (map (fn [xs] (->> xs
                          (map js/parseInt)
                          (reduce +))))
       (sort)
       (reverse)
       (take 3)
       (reduce +)))

(def testinput "1000
2000
3000

4000


5000
6000

7000
8000
9000

10000")

(comment 
  (solve-p1 testinput)
  (solve-p1 (load-input 1))
  (solve-p2 (load-input 1)))
