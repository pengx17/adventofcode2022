(ns advent.day5
  (:require [cljs.tools.reader :refer [read-string]]
            [clojure.string :as string]
            [advent.io :refer [load-input]]))

;; https://adventofcode.com/2022/day/5


(def test-input "    [D]    
[N] [C]    
[Z] [M] [P]
 1   2   3 

move 1 from 2 to 1
move 3 from 1 to 3
move 2 from 2 to 1
move 1 from 1 to 2
")

(defn crates-strings->vec [crates-strings idx]
  (reduce (fn [accum row] (let [c (nth row (inc (* 4 idx)))]
                            (if (not= " " c) (conj accum c) accum))) [] crates-strings))

(defn do-move [reverse? crates move-string]
  (let [[_ n from-idx to-idx] (re-matches #"move\s+(\d+)\s+from\s+(\d+)\s+to\s+(\d+)" move-string)
        n (read-string n)
        from-idx (dec (read-string from-idx))
        to-idx (dec (read-string to-idx))
        from (nth crates from-idx)
        to (nth crates to-idx)
        new-from (drop n from)
        new-to (concat (if reverse?
                         (reverse (take n from))
                         (take n from)) to)]
    (assoc crates
           from-idx new-from
           to-idx new-to)))

(defn solve [input reverse?]
  (let [[crates-strings _ moves] (->> (string/split-lines input)
                                      (partition-by (partial = "")))
        moves (filter (complement empty?) moves)
        crates-count (/ (inc (count (last crates-strings))) 4)
        crates (for [idx (range crates-count)]
                 (crates-strings->vec (drop-last crates-strings) idx))
        crates (reduce (partial do-move reverse?) (vec crates) moves)]
    (->> (map first crates)
         (reduce str))))

(comment (solve test-input true)
         (solve (load-input 5) true)

         (solve test-input false)
         (solve (load-input 5) false)
         )