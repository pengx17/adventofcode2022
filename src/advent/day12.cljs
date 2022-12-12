(ns advent.day12
  (:require [advent.io :refer [load-input write-debug]]
            [cljs.core :as c]
            [goog.string :refer [format]]
            [goog.string.format]
            [clojure.string :as string]))

(def test-input "Sabqponm
abcryxxl
accszExk
acctuvwj
abdefghi")

(defn ->map [input]
  (let [lines (string/split-lines input)
        rows (count lines)
        cols (count (first lines))]
    [(into
      {}
      (for [col (range cols)
            row (range rows)]
        [[col row] (nth (nth lines row) col)]))
     [cols rows]]))

(defn get-special [m special]
  (some (fn [[pos d]] (if (= d special) pos false)) m))

(defn ->level [c]
  (let [c (case c
            "E" "z"
            "S" "a"
            c)]
    (- (.charCodeAt c 0) (.charCodeAt "a" 0))))

(defn can-visist? [m from to]
  (when (and (m from) (m to))
    (let [f (->level (m from))
          t (->level (m to))]
      (<= (- t f) 1))))

(defn solve-p1 [input]
  (let [[m] (->map input)
        S (get-special m "S")
        E (get-special m "E")
        visited-dist (loop [queue [[E 0]]
                            visited #{E}
                            visited-dist {E 0}]
                       (if (or (visited S) (empty? queue))
                         visited-dist
                         (let [cur (first queue)
                               [[x y] counter] cur
                               nexts (->> (for [dx [-1 0 1]
                                                dy [-1 0 1]
                                                :when (and (not (and (zero? dx) (zero? dy)))
                                                           (or (zero? dx) (zero? dy)))]
                                            [(+ x dx) (+ y dy)])
                                          (filter #(not (visited %)))
                                          (filter #(can-visist? m % [x y])))
                               nexts-with-score (map (fn [next] [next (inc counter)]) nexts)]
                           (recur (into (vec (rest queue)) nexts-with-score)
                                  (into visited nexts)
                                  (into visited-dist nexts-with-score)))))]
    (prn (visited-dist S))
    visited-dist))

(defn solve-p2 [input]
  (let [[m] (->map input)
        S (get-special m "S")
        E (get-special m "E")
        visited-dist (loop [queue [[E 0]]
                            visited #{E}
                            visited-dist {E 0}]
                       (if (or (visited S) (some #(= (m %) "a") visited) (empty? queue))
                         visited-dist
                         (let [cur (first queue)
                               [[x y] counter] cur
                               nexts (->> (for [dx [-1 0 1]
                                                dy [-1 0 1]
                                                :when (and (not (and (zero? dx) (zero? dy)))
                                                           (or (zero? dx) (zero? dy)))]
                                            [(+ x dx) (+ y dy)])
                                          (filter #(not (visited %)))
                                          (filter #(can-visist? m % [x y])))
                               nexts-with-score (map (fn [next] [next (inc counter)]) nexts)]
                           (recur (into (vec (rest queue)) nexts-with-score)
                                  (into visited nexts)
                                  (into visited-dist nexts-with-score)))))]
    (prn (some (fn [[pos score]] (if (= (m pos) "a") score false)) visited-dist))
    visited-dist))

(defn print-visisted [input visited]
  (let [[m [w h]] (->map input)]
    (doseq [y (range h)]
      (write-debug
       (str (string/join " " (for [x (range w)]
                               (if-let [score (visited [x y])]
                                 (format "%2d-%3d" (->level (m [x y])) score)
                                 (format "%2d-   " (->level (m [x y]))))))
            "\n")
       "test"))))

(comment (solve-p1 test-input)
         (solve-p1 (load-input 12))
         (print-visisted (load-input 12) (solve-p1 (load-input 12)))
         (print-visisted (load-input 12) (solve-p2 (load-input 12)))
         (solve-p2 (load-input 12)))

