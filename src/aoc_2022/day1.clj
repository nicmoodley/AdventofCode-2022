(ns day1
  (:require
   [aoc-2022.core :as a22.core]
   [clojure.walk :as walk]))

;; this is a lie, I accidentally vanished my original code ..
;; reproduced by memory

(def input (->> (aoc-2022.core/read-input 1)
                (partition-by #(= "" %))
                (remove #(= [""] %))
                (walk/postwalk (fn [node] (if (seq? node) node
                                             (Long/parseLong node))))))

(defn part-one []
  (->> input
       (map (partial reduce +))
       (apply max)))

(defn part-two []
  (->> input
       (map (partial reduce +))
       (sort >)
       (take 3)
       ((partial reduce +))))

(defn run []
  (println "Part 1:" (part-one))
  (println "Part 2:" (part-two)))
