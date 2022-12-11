(ns advent.day9 
  (:require [clojure.string :as string]
            [advent.io :refer [load-input]]))

(def test-input "R 4
U 4
L 3
D 1
R 4
D 1
L 5
R 2")

(defn move-head [[x y] ins]
  (let [[dir step] (string/split ins #" ")
        step (int step)]
    (case dir
      "U" [x (+ y step)]
      "D" [x (- y step)]
      "L" [(- x step) y]
      "R" [(+ x step) y])))

(defn move-head-diff [ins]
  (let [[dir step] (string/split ins #" ")
        step (int step)]
    (case dir
      "U" [0 step]
      "D" [0 (- step)]
      "L" [(- step) 0]
      "R" [step 0])))

(defn near? [[x y] [x' y']]
  (let [diff [(- x x') (- y y')]]
    (every? #(and (<= % 1) (>= % -1)) diff)))

(defn chase-tail [[x y] [x' y']]
  (let [[dx dy] [(- x x') (- y y')]]
    (cond
      (near? [x y] [x' y']) [x' y']
      (and (> dx 0) (> dy 0)) [(inc x') (inc y')]
      (and (< dx 0) (< dy 0)) [(dec x') (dec y')]
      (and (> dx 0) (< dy 0)) [(inc x') (dec y')]
      (and (< dx 0) (> dy 0)) [(dec x') (inc y')]
      (> dx 0) [(inc x') y']
      (> dy 0) [x' (inc y')]
      (< dx 0) [(dec x') y']
      (< dy 0) [x' (dec y')])))

(defn chase-trail [trail]
  (loop [trail trail
         trail' [[0 0]]
         tail [0 0]]
    (if (seq trail)
      (let [tail' (chase-tail (first trail) tail)]
        (recur (rest trail) (conj trail' tail') tail'))
      trail')))

(defn run-move [[dx dy] h t]
  (let [x? (zero? dy)
        p? (if x? (pos? dx) (pos? dy))
        s (if x? (abs dx) (abs dy))]
    (loop [c 0
           h h
           t t
           trail []]
      (if (= c s)
        [h t trail]
        (let [h' (update h (if x? 0 1) (if p? inc dec))
              t' (chase-tail h' t)]
          (recur (inc c) h' t' (conj trail t)))))))

(defn solve-p1
  [input]
  (let [moves (->> (string/split-lines input)
                   (map move-head-diff))]
    (loop [h [0 0]
           t [0 0]
           moves moves
           trail []]
      (if (seq moves) 
        (let [[h' t' trail'] (run-move (first moves) h t)]
          (recur h' t' (rest moves) (concat trail trail')))
        trail))))

(defn solve-p2
  [input]
  (loop [c 1
         trail (solve-p1 input)]
    (if (= c 9)
      trail
      (recur (inc c) (chase-trail trail)))))

(defn count-trail [trail]
  (count (set trail)))

(defn print-trail [trail]
  (for [row (range 50)]
    (string/join (for [col (range 50)]
                   (if (trail [(- col 25) (- 25 row)]) "#" ".")))))

(comment
  (solve-p1 test-input)
  (solve-p1 (load-input 9))
  (count-trail (solve-p2 "R 5
U 8
L 8
D 3
R 17
D 10
L 25
U 20
"))
  (count-trail (solve-p2 (load-input 9))))

(print-trail (set (solve-p2 "R 5
U 8
L 8
D 3
R 17
D 10
L 25
U 20
")))