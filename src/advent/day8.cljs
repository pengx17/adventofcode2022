(ns advent.day8
  (:require [clojure.string :as string]
            [advent.io :refer [load-input]]))

(def test-input "30373
25512
65332
33549
35390")

(defn four-directions [[x y] [w h]]
  (let [l (for [x0 (reverse (range x))] [x0 y])
        r (for [x0 (range (inc x) w)] [x0 y])
        t (for [y0 (reverse (range y))] [x y0])
        b (for [y0 (range (inc y) h)] [x y0])]
    [l r t b]))

(defn invisible? [stats [x y] [w h]]
  (let [c (stats [x y])
        lrtb (four-directions [x y] [w h])]
    (every? (fn [xys] (some (fn [xy] (when (<= c (get stats xy -1)) xy)) xys)) lrtb)))

(defn count-dir [stats xy dir]
  (let [c (stats xy)]
    (loop [dir dir
           count 0]
      (if (and (seq dir) (< (stats (first dir)) c))
        (recur (rest dir) (inc count))
        (if (seq dir) (inc count) count)))))

(defn count-dirs [stats xy dirs]
  (reduce #(* %1 (count-dir stats xy %2)) 1 dirs))

(defn input->map [input]
  (let [lines (string/split-lines input)
        rows (count lines)
        cols (count (first lines))]
    [(->> (for [row (range rows)
                 col (range cols)]
             [[row col] (int (nth (nth lines row) col))])
           (into {})) [cols rows]]))

(defn solve-p1 [input]
  (let [[stats [w h]] (input->map input)]
    (->> (map #(invisible? stats (first %) [w h]) stats)
         (reduce #(+ (if %2 0 1) %1) 1))))

(defn solve-p2 [input]
  (let [[stats [w h]] (input->map input)]
    (->> (map #(let [xy (first %)
                     dirs (four-directions xy [w h])]
                 (count-dirs stats xy dirs)) stats)
         (apply max))))

(comment (solve-p1 test-input)
         (solve-p1 (load-input 8))
         (solve-p2 test-input)
         (solve-p2 (load-input 8)))
