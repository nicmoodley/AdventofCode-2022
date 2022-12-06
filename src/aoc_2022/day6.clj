(ns day6
  (:require
   [aoc-2022.core :as a22.core]
   [clojure.string :as string]))

(def input (string/trim-newline (a22.core/read-input 6 nil)))

(defn find-marker-position [marker-length curr-position signal]
  (let [chars (take marker-length (drop curr-position signal))]
    (if (apply distinct? chars)
      (+ marker-length curr-position)
      (recur marker-length (inc curr-position) signal))))

(defn part-one []
  (find-marker-position 4 0 input))

(defn part-two []
  (find-marker-position 14 0 input))

(defn run []
  (println "Part 1:" (part-one))
  (println "Part 2:" (part-two)))
