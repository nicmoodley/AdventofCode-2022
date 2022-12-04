(ns day4
  (:require
   [aoc-2022.core :as a22.core]
   [clojure.set :as set]
   [clojure.string :as string]))

(def input (->> (a22.core/read-input 4)
                (map #(string/split % #","))
                flatten))

(defn interval-string->range-set [s]
  (let [[start end] (map parse-long (string/split s #"-"))]
    (->> (range start (inc end))
         (into #{}))))

(defn has-no-work? [set-pair]
  (or (apply set/superset? set-pair) (apply set/subset? set-pair)))

(defn no-shared-work? [set-pair]
  (empty? (apply set/intersection set-pair)))

(defn part-one []
  (->> input
       (map interval-string->range-set)
       (partition 2)
       (filter has-no-work?)
       count))

(defn part-two []
  (->> input
       (map interval-string->range-set)
       (partition 2)
       (remove no-shared-work?)
       count))

(defn run []
  (println "Part 1:" (part-one))
  (println "Part 2:" (part-two)))
