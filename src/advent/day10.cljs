(ns advent.day10
  (:require [clojure.string :as string]
            [advent.io :refer [load-input]]))

(def test-input "addx 15
addx -11
addx 6
addx -3
addx 5
addx -1
addx -8
addx 13
addx 4
noop
addx -1
addx 5
addx -1
addx 5
addx -1
addx 5
addx -1
addx 5
addx -1
addx -35
addx 1
addx 24
addx -19
addx 1
addx 16
addx -11
noop
noop
addx 21
addx -15
noop
noop
addx -3
addx 9
addx 1
addx -3
addx 8
addx 1
addx 5
noop
noop
noop
noop
noop
addx -36
noop
addx 1
addx 7
noop
noop
noop
addx 2
addx 6
noop
noop
noop
noop
noop
addx 1
noop
noop
addx 7
addx 1
noop
addx -13
addx 13
addx 7
noop
addx 1
addx -33
noop
noop
noop
addx 2
noop
noop
noop
addx 8
noop
addx -1
addx 2
addx 1
noop
addx 17
addx -9
addx 1
addx 1
addx -3
addx 11
noop
noop
addx 1
noop
addx 1
noop
noop
addx -13
addx -19
addx 1
addx 3
addx 26
addx -30
addx 12
addx -1
addx 3
addx 1
noop
noop
noop
addx -9
addx 18
addx 1
addx 2
noop
noop
addx 9
noop
noop
noop
addx -1
addx 2
addx -37
addx 1
addx 3
noop
addx 15
addx -21
addx 22
addx -6
addx 1
noop
addx 2
addx 1
noop
addx -10
noop
noop
addx 20
addx 1
addx 2
addx 2
addx -6
addx -11
noop
noop
noop")

(defn register-positions [input]
  (let [insts (string/split-lines input)
        rec (loop [insts insts
                   reg   1
                   rec   [0]]
              (if (seq insts)
                (let [inst (first insts)
                      [_ num] (re-matches #"addx\s(.+)" inst)
                      num (int num)
                      new-reg (+ reg num)]
                  (recur (rest insts)
                         new-reg
                         (concat rec (if (not= inst "noop") [reg new-reg] [reg]))))
                rec))]
    rec))

(defn solve-p1 [input]
  (let [rec (register-positions input)]
    (->> (map #(* % (nth rec (dec %))) [20 60 100 140 180 220])
         (reduce +))))


(defn solve-p2 [input]
  (map string/join (partition 40
                              (let [rec (register-positions input)]
                                (for [i (range 1 241)]
                                  (let [pos (nth rec (dec i))
                                        i' (mod i 40)]
                                    (if (and (>= i' pos) (< i' (+ pos 3)))
                                      "#"
                                      ".")))))))

(comment
  (solve-p1 (load-input 10))
  (solve-p2 (load-input 10)))

