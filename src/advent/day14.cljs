(ns advent.day14
  (:require [cljs.core :as c]
            [advent.io :refer [load-input write-debug]]
            [goog.string.format]
            [clojure.string :as string]))

(def test-input "498,4 -> 498,6 -> 496,6
503,4 -> 502,4 -> 502,9 -> 494,9
450,11 -> 550,11")

(defn input-line->rocks [line]
  (->> (string/split line #" -> ")
       (map #(string/split % ","))
       (map #(map int %))
       ((fn [pos] (map (fn [a b] [a b]) pos (rest pos))))))

(defn at-line? [[x y] [[ax ay] [bx by]]]
  (or
   (and (= ax bx x) (>= y (min ay by)) (<= y (max ay by)))
   (and (= ay by y) (>= x (min ax bx)) (<= x (max ax bx)))))

(defn at-rocks? [pos rocks]
  (rocks pos))

(defn parse-input [input]
  (->> (string/split-lines input)
       (map input-line->rocks)
       (reduce into)
       (set)))

(defn prepare-rocks-map [rocks w h]
  (into {} (for [j (range 0 h)
                 i (range 0 w)
                 :let [x (+ i (- 500 (int (/ w 2))))
                       y j]]
             [[x y]
              (some (fn [line]
                      (when (at-line? [x y] line) true)) rocks)])))

(comment 
  (parse-input (load-input 14))
  (count (parse-input (load-input 14)))
  (prepare-rocks-map (parse-input (load-input 14)) 500 200))


(def abyss-depth 200) ;; don't bother calculate it ...

(defn next-in-order [[x y]]
  [[x (inc y)]
   [(dec x) (inc y)]
   [(inc x) (inc y)]])

(defn pick-next-in-order [rocks sands pos]
  (some (fn [next] (when (and (not (at-rocks? next rocks))
                              (not (sands next))) next))
        (next-in-order pos)))

(defn drop-sand [rocks sands pos]
  (loop [pos' pos]
    (if (> (second pos') abyss-depth)
      :abyss
      (let [next (pick-next-in-order rocks sands pos')]
        (if next (recur next) pos')))))

(defn drop-sands [rocks]
  (loop [sands {}]
    (print ".")
    (let [next (drop-sand rocks sands [500 0])]
      (if (#{:abyss [500 0]} next)
        sands
        (recur (assoc sands next true))))))

(defn solve-p1 [input]
  (-> (parse-input input)
      (prepare-rocks-map 500 200)
      (drop-sands)
      (count)))

(defn solve-p2 [input]
  (-> (parse-input (str input "\n0,168 -> 1000,168"))
      (prepare-rocks-map 500 200)
      (drop-sands)
      (count)
      (inc)))

(defn pr-rocks [rocks sands w h filename]
  (doseq [j (range h)]
    (write-debug
     (str
      (string/join
       (for [i (range w)
             :let [x (+ 494 i)
                   y j]]
         (if (and (= x 500) (= y 0))
           "+"
           (cond (rocks [x y]) "#"
                 (sands [x y]) "o"
                 :else ".")))) "\n")
     filename)))

(comment
  (drop-sands (parse-input test-input))
  (solve-p1 test-input)
  (solve-p1 (load-input 14))
  (solve-p2 (load-input 14))
  
  (drop-sand (parse-input test-input) {[500 8] true} [500 0])

  (pr-rocks (parse-input test-input)
            (drop-sands (parse-input test-input))
            10 10 "test-14"))
