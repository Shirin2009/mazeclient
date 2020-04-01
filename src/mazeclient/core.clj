(ns mazeclient.core
  (:require [clojure.set :refer [intersection select]]
            [clojure.string :as str]
            [next.jdbc :as jdbc]))



;;test grid to run locally
(def sample-maze [
                  [[:e] [:s :w :e] [:w :e] [:w :s] [:s]]
                  [[:e :s] [:e :n :w] [:s :w] [:n :e] [:w :n]]
                  [[:n :e] [:w] [:s :n :e] [:w] [:s]]
                  [[:s] [:s] [:s :n :e] [:w :e] [:w :n :s]]
                  [[:e :n] [:n :w :e] [:w :n] [:e] [:n :w]]
                  ])



;;A test grid for running locally
(def sample-grid [[
                 {:n 1, :e 0, :s 0, :w 0}
                 {:n 1, :e 0, :s 0, :w 0}
                 {:n 1, :e 0, :s 0, :w 0}
                 {:n 1, :e 0, :s 0, :w 0}]
                [{:n 1, :e 0, :s 1, :w 0}
                 {:n 0, :e 1, :s 1, :w 1}
                 {:n 0, :e 1, :s 0, :w 1}
                 {:n 1, :e 1, :s 1, :w 1}]
                [{:n 1, :e 0, :s 1, :w 0}
                 {:n 1, :e 1, :s 1, :w 0}
                 {:n 1, :e 0, :s 0, :w 1}
                 {:n 1, :e 0, :s 1, :w 0}]
                [{:n 0, :e 1, :s 1, :w 0}
                 {:n 0, :e 0, :s 0, :w 1}
                 {:n 0, :e 1, :s 1, :w 1}
                 {:n 0, :e 0, :s 1, :w 1}
                 ]])



;;check the paths
(defn check-path [direction [row col]max-row max-col]
  (case direction
    [:n 1] (if (< (inc row)max-row) [(inc row) col] nil)
    [:s 1] (if (>= (dec row)0) [(dec row) col]nil)
    [:e 1] (if (< (inc col)max-col) [row (inc col)]nil)
    [:w 1] (if (>= (dec col)0 [row (dec col)]nil))nil))



;;check and finds the empty neighbours that not calculated yet
(defn neighbors [maze position neighbors]
  (filter #(nil? (get-in maze %))
          (map #(check-path % position (count maze) (count (first maze))) neighbors)))



;;create distance calculation grid
(defn initialize-distance-grid [maze]
  (vec (repeat(vec (repeat (count (first maze)) nil)))))



;;validate distance
(defn validate-distance [distance, prev-distance]
  (if (nil? prev-distance) distance (if (<distance prev-distance)
                                      distance prev-distance)))



;;calculate distances to each position from starting position
(defn distances ([maze] (distances maze [0 0] (initialize-distance-grid maze) 0))
  ([maze pos dist-grid distance]
   (reduce #(distances maze %2 %1 (inc distance))
           (assoc-in dist-grid pos (validate-distance distance (get-in dist-grid pos)))
           (neighbors dist-grid pos (get-in maze pos)))))



;;get the shortest path in the maze
(defn shortest-path [maze point]
  (def distance (distances maze))
  (def path [point])
  (reverse (loop [current-path path, [x y]point]
             (if (= [x y] [0 0])
               current-path (recur (conj current-path (get-lowest-neighbor distance [x y]))
                                   (get-lowest-neighbor distances [x y]))))))



;;;longest path
;;(defn farthest-pos
;;  ([maze] (farthest-pos maze [0 0]))
;;  ([maze pos] (let [dist (distances maze pos)]
;;     (second (last(sort-by first
;;                  (for [[x row] (map-indexed vector dist)
;;                        [y v] (map-indexed vector row)]
;;                    [v [x y]])))))))
;;
;;
;;(defn longest-path
;;  [maze](let [farthest (farthest-pos maze)
;;        and-back (farthest-pos maze farthest)]
;;    (solution maze and-back farthest)))