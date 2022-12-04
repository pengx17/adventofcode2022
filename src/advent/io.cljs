(ns advent.io
  (:require ["fs" :as fs]))

(defn load-input [day]
  (fs/readFileSync (str "./inputs/day" day) #js {:encoding "utf8"}))

(comment (load-input 1))
