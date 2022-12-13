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



(defn do-compare [left right & {:keys [compare-single?] :as opts}]
  (let [res (cond (and (int? left) (int? right))
                  (<= left right)

                  (and (coll? left) (coll? right))
                  (loop [left' left
                         right' right]
                    (cond
                      (and (= :left compare-single?) (empty? left'))
                      true

                      (and (= :right compare-single?) (empty? right'))
                      true

                      (and (seq left') (not (seq right')))
                      false

                      (not (seq left'))
                      true

                      (do-compare (first left') (first right') opts)
                      (recur (rest left') (rest right'))

                      :else false))

                  (int? left)
                  (do-compare [left] right {:compare-single? :left})

                  (int? right)
                  (do-compare left [right] {:compare-single? :right}))]
    (prn "comparing" left right res)
    res))

(defn solve-p1 [input]
  (->> (string/split-lines input)
       (partition-all 3)
       (map #(take 2 %))
       (map (fn [lines] (map read-string lines)))
       (map-indexed (fn [idx lr] (if (apply do-compare lr) (inc idx) -1)))
       (filter pos?)
       (#(do (prn %) %))
       (reduce +)))

(comment (solve-p1 test-input)
         (solve-p1 (load-input 13)))