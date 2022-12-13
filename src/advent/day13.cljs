(ns advent.day13
  (:require [cljs.core :as c]
            [advent.io :refer [load-input]]
            [cljs.tools.reader :refer [read-string]]
            [goog.string.format]
            [clojure.string :as string]))

(def test-input "[1,1,3,1,1]
[1,1,5,1,1]

[[1],[2,3,4]]
[[1],4]

[9]
[[8,7,6]]

[[4,4],4,4]
[[4,4],4,4,4]

[7,7,7,7]
[7,7,7]

[]
[3]

[[[]]]
[[]]

[1,[2,[3,[4,[5,6,7]]]],8,9]
[1,[2,[3,[4,[5,6,0]]]],8,9]")


(defn do-compare [left right]
  (let [res (cond (and (int? left) (int? right))
                  (compare left right)

                  (and (coll? left) (coll? right))
                  (loop [left' left
                         right' right]
                    (cond
                      (and (empty? left') (seq right'))
                      -1

                      (and (empty? left') (empty? right'))
                      0

                      (and (seq left') (empty? right'))
                      1

                      :else
                      (case (do-compare (first left') (first right'))
                        -1 -1
                        1 1
                        0 (recur (rest left') (rest right')))))

                  (int? left)
                  (do-compare [left] right)

                  (int? right)
                  (do-compare left [right]))]
    res))

(defn solve-p1 [input]
  (->> (string/split-lines input)
       (partition-all 3)
       (map #(take 2 %))
       (map (fn [lines] (map read-string lines)))
       (map-indexed (fn [idx lr] (if (= (apply do-compare lr) -1) (inc idx) -1)))
       (filter pos?)
       (#(do (prn %) %))
       (reduce +)))

(defn solve-p2 [input]
  (->> (string/split-lines input)
       (partition-all 3)
       (map #(take 2 %))
       (mapcat (fn [lines] (map read-string lines)))
       (concat [[[2]] [[6]]])
       (sort-by identity do-compare)
       (map-indexed (fn [idx item] [item idx]))
       (into {})
       (#(map (fn [k] (inc (% k))) [[[2]] [[6]]]))
       (reduce *)))

(comment (solve-p1 test-input)
         (solve-p1 (load-input 13))
         (solve-p2 test-input)
         (solve-p2 (load-input 13)))
