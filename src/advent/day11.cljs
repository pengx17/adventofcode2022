(ns advent.day11
  (:require [advent.io :refer [load-input]]
            [cljs.tools.reader :refer [read-string]]
            [clojure.string :as string]))

(def test-input "Monkey 0:
  Starting items: 79, 98
  Operation: new = old * 19
  Test: divisible by 23
    If true: throw to monkey 2
    If false: throw to monkey 3

Monkey 1:
  Starting items: 54, 65, 75, 74
  Operation: new = old + 6
  Test: divisible by 19
    If true: throw to monkey 2
    If false: throw to monkey 0

Monkey 2:
  Starting items: 79, 60, 97
  Operation: new = old * old
  Test: divisible by 13
    If true: throw to monkey 1
    If false: throw to monkey 3

Monkey 3:
  Starting items: 74
  Operation: new = old + 3
  Test: divisible by 17
    If true: throw to monkey 0
    If false: throw to monkey 1")

(defn model-monkey
  [[l0 l1 l2 l3 l4 l5]]
  (let [[_ id] (re-matches #"Monkey (.*):" l0)
        [_ items] (re-matches #"\s*Starting items: (.*)" l1)
        [_ op num] (re-matches #"\s*Operation: new = old (.+) (.+)" l2)
        [_ div] (re-matches #"\s*Test: divisible by (.*)" l3)
        [_ truthy] (re-matches #"\s*If true: throw to monkey (.*)" l4)
        [_ falsy] (re-matches #"\s*If false: throw to monkey (.*)" l5)]
    {:id (int id)
     :items (map #(read-string (string/trim %)) (string/split items ","))
     :operation (fn [old] ((if (= op "*") * +) (int old) (if (= num "old") (int old) (int num))))
     :divisible (int div)
     :truthy (int truthy)
     :falsy (int falsy)}))

(defn run-round [monkeys items inspects]
  (loop [monkeys monkeys
         new-dest items
         inspects inspects]
    (if (seq monkeys)
      (let [{:keys [id operation divisible truthy falsy]} (first monkeys)
            no (count (get new-dest id))
            new-dest' (reduce (fn [accum item]
                                (let [a (operation item)
                                      b (/ a 3)
                                      c (int b)
                                      d (mod c divisible)
                                      dest (if (zero? d) truthy falsy)]
                                  (update accum
                                          dest #(concat % [c]))))
                              (dissoc new-dest id)
                              (get new-dest id))]
        (recur (rest monkeys) new-dest' (update inspects id #(+ no %))))
      [new-dest inspects])))

(defn run-round-2 [monkeys items inspects]
  (loop [monkeys monkeys
         new-dest items
         inspects inspects]
    (if (seq monkeys)
      (let [{:keys [id operation divisible truthy falsy]} (first monkeys)
            no (count (get new-dest id))
            new-dest' (reduce (fn [accum item]
                                (let [a (operation item)
                                      d (mod a divisible)
                                      dest (if (zero? d) truthy falsy)]
                                  (update accum
                                          dest #(concat % [a]))))
                              (dissoc new-dest id)
                              (get new-dest id))]
        (recur (rest monkeys) new-dest' (update inspects id #(+ no %))))
      [new-dest inspects])))

(defn solve-p1 [input]
  (let [monkeys (->> (string/split-lines input)
                     (partition-all 7)
                     (map model-monkey))
        items (into {} (map (fn [monkey] [(:id monkey) (:items monkey)]) monkeys))]
    (loop [c 0
           items items
           inspects {}]
      (if (< c 20)
        (let [[items inspects] (run-round monkeys items inspects)]
          (recur (inc c) items inspects))
        (reduce * (take 2 (reverse (sort (vals inspects)))))))))

;; fixme: number overflow
(defn solve-p2 [input]
  (let [monkeys (->> (string/split-lines input)
                     (partition-all 7)
                     (map model-monkey))
        items (into {} (map (fn [monkey] [(:id monkey) (:items monkey)]) monkeys))]
    (loop [c 0
           items items
           inspects {}]
      (if (< c 20)
        (let [[items inspects] (run-round-2 monkeys items inspects)]
          (recur (inc c) items inspects))
        (do (prn inspects)
            (reduce * (take 2 (reverse (sort (vals inspects))))))))))

(comment (solve-p1 (load-input 11))
         (solve-p2 test-input))