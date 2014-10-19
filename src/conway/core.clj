(ns conway.core
  (:require [clojure.tools.cli :refer [parse-opts]])
  (:gen-class))

; helpers

(defn mapv-indexed
  [f coll]
  (loop [idx 0
         v (transient [])
         coll coll]
    (if-let [x (first coll)]
      (recur (inc idx) (conj! v (f idx x)) (rest coll))
      (persistent! v))))

; core

(defn cell
  []
  {:alive false})

(defn row
  [n]
  (into [] (take n (repeatedly cell))))

(defn grid
  [n]
  (into [] (take n (repeatedly (partial row n)))))

(defn cell-at
  [grid i j]
  (get (get grid i) j))

(defn neighbours
  "Given a grid, returns the neighbourhood."
  [grid i j]
  {:pre [(<= 0 i (- (count grid) 1))
         (<= 0 j (- (count (first grid)) 1))]}
  (let [cell-at (partial cell-at grid)]
    (remove nil? [(cell-at i (- j 1)) (cell-at i (+ j 1))
                  (cell-at (- i 1) j) (cell-at (+ i 1) j)
                  (cell-at (- i 1) (- j 1)) (cell-at (+ i 1) (+ j 1))
                  (cell-at (+ i 1) (- j 1)) (cell-at (- i 1) (+ j 1))])))

(defn alive?
  "Returns if cell is alive."
  [cell]
  (:alive cell))

(defn kill
  [cell]
  (assoc cell :alive false))

(defn make-alive
  [cell]
  (assoc cell :alive true))

(def count-pred
  "Given coll, it will filter with pred and then count."
  (comp count filter))

(def count-alive
  (partial count-pred alive?))

(defn transit-alive
  [cell n-alive]
  (condp contains? n-alive
    #{0 1} (kill cell)
    #{2 3} cell
    (kill cell)))

(defn transit-dead
  [cell n-alive]
  (if (= n-alive 3)
    (make-alive cell)
    cell))

(defn transit-cell
  [grid i j cell]
  (let [n-alive (count-alive (neighbours grid i j))]
    (if (alive? cell)
      (transit-alive cell n-alive)
      (transit-dead cell n-alive))))

(defn transit-row
  [grid i row]
  (let [f (partial transit-cell grid i)]
    (mapv-indexed f row)))

; api

(defn transit
  "Given a grid, returns a new grid where:
  * Any live cell with fewer than two live neighbours dies, as if caused by under-population.
  * Any live cell with two or three live neighbours lives on to the next generation.
  * Any live cell with more than three live neighbours dies, as if by overcrowding.
  * Any dead cell with exactly three live neighbours becomes a live cell, as if by reproduction.
  "
  [grid]
  (mapv-indexed (partial transit-row grid) grid))

; printing

(defn row->str
  [row]
  (map #(if (alive? %) "x" "_") row))

(defn grid->str
  [grid]
  (map row->str grid))

(defn print-grid
  [grid]
  (doseq [row (grid->str grid)]
    (println (apply str (interpose " " row)))))

(defn assoc-in-grid
  [grid ks-coll value]
  (loop [ks-coll ks-coll
         new-grid grid]
    (if-let [ks (first ks-coll)]
      (recur (rest ks-coll) (assoc-in new-grid ks value))
      new-grid)))

(def cli-options
  [["-t" "--times" "Run n times."
    :parse-fn #(Integer/parseInt %)
    :default 10]])

(defn -main [& args]
  (let [{:keys [options]} (parse-opts args cli-options)]
    ; FIXME Read from CLI
    (loop [i 60
           new-grid (assoc-in-grid
                      (grid 10)
                      [[0 1 :alive]
                       [1 2 :alive]
                       [2 0 :alive]
                       [2 1 :alive]
                       [2 2 :alive]]
                      true)]
      (if (pos? i)
        (do
          (println (str "tick " i))
          (print-grid new-grid)
          (Thread/sleep 1000)
          (recur (dec i) (transit new-grid)))))))

