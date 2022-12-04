(ns day3
  (:require
   [aoc-2022.core :as a22.core]
   [clojure.set :as set]
   [clojure.string :as string]))

(def input (a22.core/read-input 3))

(def alphabet "abcdefghijklmnopqrstuvwxyz")
(def priorities-map
  (merge (zipmap (seq alphabet) (range 1 27))
         (zipmap (seq (string/upper-case alphabet)) (range 27 53))))

(defn common-item-in-rucksack [s]
  (let [n (count s) s (seq s)]
    (->> (split-at (/ n 2) s)
         (map (partial into #{}))
         (apply set/intersection)
         first)))

(defn common-item-in-group [group]
  (->> (map (comp (partial into #{}) seq) group)
       (apply set/intersection)
       first))

(defn part-one []
  (->> input
       (map #(get priorities-map (common-item-in-rucksack %)))
       ((partial reduce +))))

(defn part-two []
  (->> input
       (partition 3)
       (map (comp (partial get priorities-map) common-item-in-group))
       ((partial reduce +))))

(defn run []
  (println "Part 1:" (part-one))
  (println "Part 2:" (part-two)))
