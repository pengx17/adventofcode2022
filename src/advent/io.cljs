(ns advent.io
  (:require ["fs" :as fs]))

(defn load-input [day]
  (fs/readFileSync (str "./inputs/day" day) #js {:encoding "utf8"}))

(defn write-debug [content to]
  (fs/writeFileSync (str "./out/" to) content #js {:flag "a+"}))

(comment (load-input 1))
