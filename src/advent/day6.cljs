(ns advent.day6
  (:require [advent.io :refer [load-input]]))

;; https://adventofcode.com/2022/day/5


(def test-inputs ["mjqjpqmgbljsphdztnvjfqwrcgsmlb"
                  "bvwbjplbgvbhsrlpgdmjqwftvncz"
                  "nppdvjthqldpwncqszvftbrmjlhg"
                  "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg"
                  "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw"])

(defn has-repeated-char?
  [input]
  (not= (count (set input)) (count input)))

(defn solve [input window]
  (loop [n window
         input input]
    (cond
      (< (count input) window)
      nil

      (has-repeated-char? (subs input 0 window))
      (recur (inc n) (subs input 1))

      :else n)))

(defn solve-p1 [input]
  (solve input 4))

(defn solve-p2 [input]
  (solve input 14))


(comment (map solve-p1 test-inputs)
         (map solve-p2 test-inputs)
         (solve-p1 (load-input 6))
         (solve-p2 (load-input 6)))
