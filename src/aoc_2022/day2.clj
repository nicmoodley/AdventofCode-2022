(ns day2
  (:require [aoc-2022.core :as a22.core]
            [clojure.string :as string]))

(def input (->> (a22.core/read-input 2)
                (map #(string/split % #" "))))

;; Logic:
;; for {Rock->1, Paper->2, Scissors->3}, with i := opponent's play, j := my play
;; k := my result
;;     k = (i - j) mod 3
;; where k = {0->Draw, 1->Loss, 2->Win}

(defn cheat-pair->i-j-representation [pair]
  (let [ij-representation #(cond
                             (or (= "A" %) (= "X" %)) 1
                             (or (= "B" %) (= "Y" %)) 2
                             (or (= "C" %) (= "Z" %)) 3)]
    (map ij-representation pair)))

(defn cheat-pair->i-k-representation [pair]
  (let [i (first (cheat-pair->i-j-representation pair))
        result-code (second pair)
        k (cond
            (= "X" result-code) 1 ; loss
            (= "Y" result-code) 0 ; draw
            (= "Z" result-code) 2 ; win
            )]
    [i k]))

(defn i-j->k [[i j]]
  (mod (- i j) 3))

(defn k->match-score [k]
  (cond
    (= 0 k) 3 ; draw
    (= 1 k) 0 ; loss
    (= 2 k) 6 ; win
    ))

(defn i-k->j [[i k]]
  (mod (- i k) 3))

(defn j->hand-score [j]
  (if (= 0 j) 3 j))

(defn part-one []
  (->> input
       (map cheat-pair->i-j-representation)
       (map (juxt second (comp k->match-score i-j->k)))
       (map (partial reduce +))
       ((partial reduce +))))

(defn part-two []
  (->> input
       (map cheat-pair->i-k-representation)
       (map (juxt (comp j->hand-score i-k->j) (comp k->match-score second)))
       (map (partial reduce +))
       ((partial reduce +))))

(defn run []
  (println "Part 1:" (part-one))
  (println "Part 2:" (part-two)))
