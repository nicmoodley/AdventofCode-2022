(ns day5
  (:require
   [aoc-2022.core :as a22.core]
   [clojure.string :as string]))

(def input (a22.core/read-input 5))

(defn parse-line [s]
  (if-let [instructions (re-seq #"\d+" s)]
    (map parse-long instructions)
    (map str (take-nth 4 (rest s)))))

(defn read-stacks [input]
  (let [stack-height
        (first (keep-indexed #(when (string/starts-with? %2 " 1") %1) input))]
    (->> (map parse-line (take stack-height input))
         (apply map vector) ; transpose
         (map #(remove string/blank? %))
         (zipmap (range 1 (+ stack-height 2)))
         (into (sorted-map)))))

(defn read-instructions [input]
  (->> (filter #(string/starts-with? % "m") input)
       (map parse-line)))

(defn move-crates [reverse? stacks instructions]
  (let [[move-number A B] instructions
        chosen-crates (take move-number (get stacks A))
        chosen-crates (if reverse? (reverse chosen-crates) chosen-crates)]
    (-> stacks
        (update A (partial drop move-number))
        (update B (partial concat chosen-crates)))))

(defn move-all-crates [reverse? initial-stacks instructions]
  (reduce (partial move-crates reverse?)
   initial-stacks instructions))

(defn part-one []
  (->> (move-all-crates true (read-stacks input) (read-instructions input))
       vals
       (map first)
       (apply str)))

(defn part-two []
  (->> (move-all-crates false (read-stacks input) (read-instructions input))
       vals
       (map first)
       (apply str)))

(defn run []
  (println "Part 1:" (part-one))
  (println "Part 2:" (part-two)))
