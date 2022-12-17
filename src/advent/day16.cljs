(ns advent.day16
  (:require [cljs.core :as c]
            [advent.io :refer [load-input]]
            [clojure.string :as string]))

(def test-input "Valve AA has flow rate=0; tunnels lead to valves DD, II, BB
Valve BB has flow rate=13; tunnels lead to valves CC, AA
Valve CC has flow rate=2; tunnels lead to valves DD, BB
Valve DD has flow rate=20; tunnels lead to valves CC, AA, EE
Valve EE has flow rate=3; tunnels lead to valves FF, DD
Valve FF has flow rate=0; tunnels lead to valves EE, GG
Valve GG has flow rate=0; tunnels lead to valves FF, HH
Valve HH has flow rate=22; tunnel leads to valve GG
Valve II has flow rate=0; tunnels lead to valves AA, JJ
Valve JJ has flow rate=21; tunnel leads to valve II")

(defn parse-line [line]
  (let [[_ from rate to] (re-matches #"Valve (.*) has flow rate=(.*); tunnels? leads? to valves? (.*)" line)]
    [from (int rate) (string/split to ", ")]))


(defn parse-input [input]
  (->> (string/split-lines input)
       (map parse-line)))

(defn get-path-from-visited-dist [visited-dist from to]
  (loop [cur from
         path [cur]]
    (if (= cur to)
      path
      (let [next (some (fn [[c n]] (when (= n (dec (visited-dist cur))) c)) visited-dist)]
        (recur next (conj path next))))))

(defn find-path [from->tos from to]
  (loop [queue [[to 0]]
         visited #{to}
         visited-dist {to 0}]
    (if (visited from)
      (get-path-from-visited-dist visited-dist from to)
      (let [[cur counter] (first queue)
            nexts (->> (from->tos cur)
                       (remove visited))]
        (recur (into (vec (rest queue)) (map (fn [next] [next (inc counter)]) nexts))
               (conj visited cur)
               (assoc visited-dist cur (inc counter)))))))

(defn get-from->tos [input]
  (->> (parse-input input)
       (map (fn [[from _ tos]] [from tos]))
       (into {})))

(defn get-pairs [xs]
  (into #{} (for [x xs
                  y xs
                  :when (not= x y)]
              [x y])))

(def r-visit
  (memoize
   (fn [from-to->path costs visited remaining from]
     (let [nexts (->> (keys from-to->path)
                      (filter #(= (first %) from))
                      (map second)
                      (remove visited)
                      (remove #(zero? (costs %)))
                      (map (fn [next] [next (- remaining (count (from-to->path [from next])))]))
                      (remove #(neg? (second %))))]
       (if (seq nexts)
         (mapcat (fn [[next n-remaining]]
                   (->> (r-visit from-to->path costs (conj visited next) n-remaining next)
                        (map (fn [v] (+ v (* (costs next) n-remaining))))))
                 nexts)
         [0])))))

(defn solve-p1 [input]
  (let [start-time (js/performance.now)
        lines (parse-input input)
        from->tos (get-from->tos input)
        from-to->path (->> lines
                           (map first)
                           (get-pairs)
                           (map #(into [] %))
                           (map (fn [[from to]] [[from to] (find-path from->tos from to)]))
                           (into {}))
        costs (->> (map drop-last lines)
                   (map vec)
                   (into {}))
        result (->> (r-visit from-to->path costs #{} 30 "AA")
                    (apply max))]
    (prn "time: " (- (js/performance.now) start-time) "ms")
    result))

(comment (solve-p1 test-input)
         (solve-p1 (load-input 16)))
