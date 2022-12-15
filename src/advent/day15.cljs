(ns advent.day15
  (:require [cljs.core :as c]
            [advent.io :refer [load-input write-debug]]
            [goog.string.format]
            [clojure.string :as string]))

(def test-input "Sensor at x=2, y=18: closest beacon is at x=-2, y=15
Sensor at x=9, y=16: closest beacon is at x=10, y=16
Sensor at x=13, y=2: closest beacon is at x=15, y=3
Sensor at x=12, y=14: closest beacon is at x=10, y=16
Sensor at x=10, y=20: closest beacon is at x=10, y=16
Sensor at x=14, y=17: closest beacon is at x=10, y=16
Sensor at x=8, y=7: closest beacon is at x=2, y=10
Sensor at x=2, y=0: closest beacon is at x=2, y=10
Sensor at x=0, y=11: closest beacon is at x=2, y=10
Sensor at x=20, y=14: closest beacon is at x=25, y=17
Sensor at x=17, y=20: closest beacon is at x=21, y=22
Sensor at x=16, y=7: closest beacon is at x=15, y=3
Sensor at x=14, y=3: closest beacon is at x=15, y=3
Sensor at x=20, y=1: closest beacon is at x=15, y=3
")

(defn parse-line [line]
  (->> (re-matches #"Sensor at x=(.*), y=(.*): closest beacon is at x=(.*), y=(.*)" line)
       (rest)
       (map int)
       (partition 2)))


(defn dist [s b]
  (+ (abs (- (first s) (first b)))
     (abs (- (second s) (second b)))))

(defn parse-input [input]
  (->> (string/split-lines input)
       (remove string/blank?)
       (map parse-line)))

(defn lines->beacons [lines]
  (->> (map second lines)
       (set)))

(defn in-range? [[s-pos d] pos]
  (<= (dist s-pos pos) d))

(defn in-the-range? [[l r] x] 
  (and (<= l x) (>= r x)))

(defn intersect-line [[[x y] d] lno]
  (let [dx (- d (abs (- lno y)))]
    (when-not (neg? dx) [(- x dx) (+ x dx)])))


(defn merge-intersect [[x0 x1] [x2 x3]]
  (if (or (> x0 (inc x3)) (< (inc x1) x2))
    nil [[(min x0 x2) (max x1 x3)]]))

(comment (merge-intersect [2 3] [4 5])
         (merge-intersect [2 2] [4 5])
         (merge-intersect [4 5] [2 3])
         (merge-intersect [4 5] [2 4])
         (merge-intersect [1 5] [2 4]))

(defn lines->sensor-map [lines]
  (->> (map (fn [pair] [(first pair) (apply dist pair)]) lines)
       (into {})))

(defn solve-p1-naive [input lno]
  (let [lines (parse-input input)
        beacons (lines->beacons lines)
        sensor-map (lines->sensor-map lines)]
    (loop [s -10000000
           c 0]
      (if (< s 10000000)
        (recur (inc s)
               (if (and (some #(in-range? % [s lno]) sensor-map)
                        (not (beacons [s lno])))
                 (inc c) c))
        c))))

(defn merge-all-ranges [ranges]
  (prn ranges)
  (let [sorted (sort-by first ranges)]
    (loop [last-merged (first sorted)
           sorted sorted
           result []]
      (if (seq sorted)
        (let [merge-result (merge-intersect last-merged (first sorted))]
          (if merge-result
            (recur merge-result (rest sorted) result)
            (recur (first sorted) (rest sorted) (conj result last-merged))))
        (conj result last-merged)))))

(defn get-beacons [beacons lno]
  (set (filter #(= lno (second %)) beacons)))

(defn solve-p1 [input lno]
  (let [lines (parse-input input)
        beacons (lines->beacons lines)
        sensor-map (lines->sensor-map lines)
        beacons (get-beacons beacons lno)
        ranges (->> (map #(intersect-line % lno) sensor-map)
                    (merge-all-ranges))
        beacons-at-line (filter (fn [beacon]
                                  (some #(if (in-the-range? % (first beacon)) beacon nil) ranges)) beacons)]
    (- (->> ranges
            (map #(- (second %) (first %)))
            (reduce +)
            (inc))
       (count beacons-at-line))))

(defn visualize [input]
  (let [lines (parse-input input)
        beacons (lines->beacons lines)
        sensor-map (lines->sensor-map lines)
        sensors (set (keys sensor-map))
        all-points (into beacons sensors)
        max-x (reduce (fn [acc cur] (max (first cur) acc)) 0 all-points)
        max-y (reduce (fn [acc cur] (max (second cur) acc)) 0 all-points)
        min-x (reduce (fn [acc cur] (min (first cur) acc)) 0 all-points)
        min-y (reduce (fn [acc cur] (min (second cur) acc)) 0 all-points)]
    (for [j (range (inc (- max-y min-y)))]
      (string/join (for [i (range (inc (- max-x min-x)))
                         :let [x (+ i min-x)
                               y (+ j min-y)]]
                     (cond (beacons [x y]) "B"
                           (sensors [x y]) "S"
                           :else "."))))))

(defn solve-p2 [input max-ln]
  (let [lines (parse-input input)
        sensor-map (lines->sensor-map lines)]
    (loop [lno 0]
      (if (= lno (inc max-ln))
        nil
        (let [ranges (->> (map #(intersect-line % lno) sensor-map)
                          (merge-all-ranges))]
          (prn ranges)
          (if (= 2 (count ranges))
            ranges
            ;;[(inc (last (first (sort-by first ranges)))) lno]
            (recur (inc lno))))))))

(comment (parse-input test-input)
         (solve-p1-naive test-input 10) ;; 26
         (solve-p1-naive (load-input 15) 2000000) ;; 5878678
         (solve-p1 test-input 10) ;; 26
         (solve-p1 (load-input 15) 2000000)
         
         (solve-p2 test-input 20)
         (solve-p2 (load-input 15) 4000000)
         
         (visualize test-input))