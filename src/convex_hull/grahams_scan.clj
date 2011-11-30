(ns convex-hull.grahams-scan
  (:require [geom.pt :as pt]
            [geom.seg :as seg])
  (:use [clojure.algo.monads]
        [geom.polygon-parser-m]))

(defn- three-coins-take
  "Takes a point from the polygon and puts it on the stack when appropriate."
  [poly]
  (fn [pt stack]
    (cond
     (empty? stack)
     [pt (rest poly) [(first poly)]]
     (empty? (rest stack))
     [pt (rest poly) (cons (first poly) stack)]
     :else
     (when (pt/left-turn? (second stack) (first stack) (first poly))
       [pt (rest poly) (cons (first poly) stack)]))))

(defn- three-coins-pop
  "A monadic function that is called when three-coins-take fails.
   Pops the top point from the stack.  Will be called until the
   next point on the polygon is a left-turn from the top of the stack."
  [poly]
  (fn [pt stack]
    [pt poly (rest stack)]))

(defn- three-coins [poly]
  (with-monad polygon-parser-m
    (m-plus (three-coins-take poly) (three-coins-pop poly))))

(defn- grahams-scan-helper [poly]
  ((with-monad polygon-parser-m
     (m-until empty? three-coins poly)) nil []))

(defn grahams-scan
  "Finds the convex hull of a set of points by
   the algorithm known as 'Graham's Scan'."
  [pts]
  (let [bottom-pt (apply min-key :y pts)
        dummy-seg (seg/new-seg (pt/new-pt (dec (bottom-pt :x)) (bottom-pt :y)) bottom-pt)
        sorted-pts (->> pts
                        (remove (partial = bottom-pt))
                        (map (partial seg/new-seg bottom-pt))
                        (sort-by (partial seg/angle-between-segs dummy-seg))
                        (map :e2))
        [_ _ stack] (grahams-scan-helper (cons bottom-pt sorted-pts))]
    (vec (reverse stack))))
