(ns advent.day1
  (:require [clojure.string :as string]
            [advent.io :refer [load-input]]))

;; https://adventofcode.com/2022/day/1

(defn solve [input]
  (->> input
       (string/split-lines)
       (partition-by empty?)
       (filter #(not (every? empty? %)))
       (map (fn [xs] (->> xs
                          (map js/parseInt)
                          (reduce +))))
       (apply max)))

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
  (solve testinput)
  (solve (load-input 1)))
