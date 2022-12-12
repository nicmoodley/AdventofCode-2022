(ns day8
  (:require
   [aoc-2022.core :as a22.core]
   [clojure.string :as string]
   ))

(def sample "30373
25512
65332
33549
35390")

(def input (a22.core/read-input 8))

(defn input->grid [input]
  (mapv (partial mapv (comp parse-long str)) input))

(defn transpose [x]
  (apply mapv vector x))

(defn visible-in? [n ref-trees]
  (> n (apply max ref-trees)))

(defn l-trees [grid i j]
  (subvec (nth grid i) 0 j))
(defn r-trees [grid i j]
  (subvec (nth grid i) (inc j)))

(defn visible? [grid i j]
  (let [n (nth (nth grid i) j)
        trans-grid (transpose grid)
        t-trees (l-trees trans-grid j i)
        b-trees (r-trees trans-grid j i)
        l-trees (l-trees grid i j)
        r-trees (r-trees grid i j)
        visible-in? (partial visible-in? n)]
    (if (or (visible-in? l-trees) (visible-in? r-trees)
            (visible-in? t-trees) (visible-in? b-trees))
      1 0)))

(defn visible-in-row [grid i]
  (->> (mapv (partial visible? grid i) (range 1 (dec (count (first grid)))))
       ((partial reduce +))))

(defn count-visible [grid]
  (let [n-rows (count grid)
        n-cols (count (first grid))
        n-ext-visible (- (* (+ n-rows n-cols) 2) 4)
        n-int-visible (->> (map (partial visible-in-row grid)
                                (range 1 (dec n-rows)))
                           ((partial reduce +)))]
    (+ n-int-visible n-ext-visible)))

(defn viewing-distance [curr-distance n ref-trees]
  (if (= (inc curr-distance) (count ref-trees))
    (inc curr-distance)
    (if (>= (nth ref-trees curr-distance) n)
      (inc curr-distance)
      (recur (inc curr-distance) n ref-trees))))

(defn scenic-score [grid trans-grid i j]
  (let [n (nth (nth grid i) j)
        t-trees (reverse (l-trees trans-grid j i))
        b-trees (r-trees trans-grid j i)
        r-trees (r-trees grid i j)
        l-trees (reverse (l-trees grid i j))]
    (->> [t-trees r-trees l-trees b-trees]
         (map (partial viewing-distance 0 n))
         (apply *))))

(defn scenic-in-row [grid trans-grid i]
  (->> (range 1 (dec (count trans-grid)))
       (mapv (partial scenic-score grid trans-grid i))
       (apply max)))

(defn most-scenic-score [grid]
  (let [trans-grid (transpose grid)
        n-rows (count grid)]
    (->> (map (partial scenic-in-row grid trans-grid) (range 1 (dec n-rows)))
         (apply max))))

(defn part-one []
  (count-visible (input->grid input)))

(defn part-two []
  (most-scenic-score (input->grid input)))

(defn run []
  (println "Part 1:" (part-one))
  (println "Part 2:" (part-two)))
