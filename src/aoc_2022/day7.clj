(ns day7
  (:require
   [aoc-2022.core :as a22.core]
   [clojure.string :as string]
   [clojure.walk :as walk]))

(defn categorise-line [s]
  (let [s-items (string/split s #" ")]
    (cond
      (and (= "$" (first s-items)) (= 2 (count s-items))) :ignore
      (and (= "$" (first s-items)) (= 3 (count s-items))) :cd
      :else :ls-item
      )))

(defn do-ls [dir-tree ls-info]
  (if (string/starts-with? ls-info "dir")
    (let [dir-name (second (string/split ls-info #" "))]
      (walk/postwalk
       (fn [node] (if (map? node)
                   (if (true? (:active? node))
                     (update node :items (comp vec (partial concat [{:name dir-name
                                                            :items []
                                                            :size false
                                                            :active? false}])))
                     node)
                   node))
       dir-tree))
    (let [[size name] (string/split ls-info #" ")]
      (walk/postwalk
       (fn [node] (if (map? node)
                   (if (true? (:active? node))
                     (update node :items (comp vec (partial concat [{:name name
                                                            :items nil
                                                            :size (parse-long size)
                                                            :active? false}])))
                     node)
                   node))
       dir-tree))))

(defn do-cd [dir-tree cd-info]
  (let [[_ _ cd-location] (string/split cd-info #" ")]
    (if (= ".." cd-location)
      (walk/prewalk ; ordering dictates prewalk!
       (fn [node] (if (and (map? node) (vector? (:items node))
                          (some #(true? (:active? %)) (:items node)))
                   (-> (update node :items (partial mapv #(assoc % :active? false)))
                       (assoc :active? true))
                   node))
       dir-tree)
      (walk/postwalk
       (fn [node] (if (and (map? node) (true? (:active? node))
                          (some #(= cd-location (:name %)) (:items node)))
                   (->> (assoc node :active? false)
                        (#(for [i (range (count (:items %)))
                                :let [item (nth (:items %) i)]
                                :when (and (= cd-location (:name item))
                                           (some? (:items item)))]
                            (assoc-in % [:items i :active?] true)))
                        first) ; for wraps in list, don't want that
                   node))
       dir-tree))))

(defn do-line [dir-tree s]
  (let [line-type (categorise-line s)]
    (case line-type
      :ignore dir-tree
      :ls-item (do-ls dir-tree s)
      :cd (do-cd dir-tree s))))

(defn add-directory-size [flat-tree]
  (if-let [working-dir (->> (filter #(false? (:size %)) flat-tree)
                            reverse
                            first)]
    (let [working-dir-size (reduce + (map :size (:items working-dir)))]
      (walk/postwalk (fn [node] (if (= working-dir node)
                                  (assoc node :size working-dir-size)
                                  node)) flat-tree))
    nil))

(defn add-directory-sizes [filesystem]
  (let [flat-filesystem (vec (tree-seq map? :items filesystem))]
    (reduce #(if (nil? %2) (reduced %1) %2)
            (iterate add-directory-size flat-filesystem))))

;; -----------------------------------------------------------------------------

(def input (a22.core/read-input 7))

(def home-dir {:name "/" :items [] :size false :active? true})
(def flat-fs (-> (reduce do-line home-dir (rest input))
                 add-directory-sizes))

(defn part-one []
  (->> flat-fs
       (filter #(and (some? (:items %)) (>= 100000 (:size %))))
       (map :size)
       ((partial reduce +))))

(defn part-two []
  (let [space-taken (- 70000000 (:size (first flat-fs)))
        space-needed (- 30000000 space-taken)]
    (->> flat-fs
         (filter #(and (some? (:items %))
                       (<= space-needed (:size %))))
         (sort-by :size <)
         (map :size)
         first)))

(defn run []
  (println "Part 1:" (part-one))
  (println "Part 2:" (part-two)))
