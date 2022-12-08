(ns advent.day7 
  (:require [clojure.string :as string]
            [advent.io :refer [load-input]]))

(def test-input
  "
$ cd /
$ ls
dir a
14848514 b.txt
8504156 c.dat
dir d
$ cd a
$ ls
dir e
29116 f
2557 g
62596 h.lst
$ cd e
$ ls
584 i
$ cd ..
$ cd ..
$ cd d
$ ls
4060174 j
8033020 d.log
5626152 d.ext
7214296 k")

(defn get-dir [dir cd]
  (cond (= cd "/")
        ["/"]

        (= cd "..")
        (if (> (count dir) 1)
          (vec (drop-last dir))
          ["/"])
        
        :else
        (conj dir cd)))

(defn build-fs [lines]
  (loop [lines lines
         dir ["/"]
         stats {}]
    (if (empty? lines)
      stats
      (let [head-line (first lines)
            rest-lines (rest lines)
            [_ cd] (re-matches #"\$ cd (.+)$" head-line)
            ls (re-matches #"\$ ls$" head-line)]
        (cond (some? cd)
              (recur rest-lines (get-dir dir cd) stats)

              (some? ls) ;; lookahead and get all dir stats
              (let [ls-lines (take-while #(not (string/starts-with? % "$ ")) rest-lines)]
                (when (get stats dir) (prn dir (get stats dir) (set ls-lines)))
                (recur (drop (count ls-lines) rest-lines)
                       dir
                       (assoc stats dir (set ls-lines)))))))))

(defn get-dir-size [stats dir]
  (reduce (fn [accum line] (let [[_ dir2] (re-matches #"dir (.+)$" line)
                                 [_ size _] (re-matches #"(\d+) (.+)$" line)]
                             (+ accum (if (some? dir2)
                                        (get-dir-size stats (conj dir dir2))
                                        (int size)))))
          0
          (get stats dir)))


(defn build-stats [input]
  (let [lines (->> input
                   (string/trim)
                   (string/split-lines)
                   (map string/trim))
        stats (build-fs lines)]
    stats))

(defn solve-p1 [input]
  (let [stats (build-stats input)
        dirs (keys stats)]
    (->> dirs
         (map #(get-dir-size stats %))
         (filter #(<= % 100000))
         (reduce +))))


(defn solve-p2 [input]
  (let [stats (build-stats input)
        dirs (keys stats)
        dir-size (map (fn [dir] [dir (get-dir-size stats dir)]) dirs)
        dir->size (into {} dir-size)
        root-size (dir->size ["/"])
        options (filter some? (map (fn [size]
                               (let [after-delete (+ (- 70000000 root-size) size)]
                                 (when (> after-delete 30000000) size)))
                             (vals dir->size)))]
    (reduce min options)))

(comment
  (solve-p1 test-input)
  (solve-p1 (load-input 7))

  (solve-p2 test-input)
  (solve-p2 (load-input 7))
  )
