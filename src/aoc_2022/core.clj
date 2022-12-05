(ns aoc-2022.core
  (:require [clojure.string :as string]))

(defn read-input
  ([n split?]
   (if split?
     (string/split-lines (slurp (str "resources/inputs/day" n ".txt")))
     (slurp (str "resources/inputs/day" n ".txt"))))
  ([n]
   (read-input n true)))
