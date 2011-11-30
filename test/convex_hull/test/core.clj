(ns convex-hull.test.core
  (:require [geom.pt :as pt])
  (:use [convex-hull.jarvis-march]
        [convex-hull.grahams-scan]
        [convex-hull.chans-algorithm])
  (:use [clojure.test]))

(defn cyclic-equal? [c1 c2]
  (let [len (count c1)]
    (some true? (map #(= c1 (take len (drop (- len %) (cycle c2)))) (range len)))))

(def default-ch
  [(pt/new-pt 0 0) (pt/new-pt 1 0) (pt/new-pt 1 1) (pt/new-pt 0 1)])

(defn default-pts [n]
  (let [rand-fn (fn [] (rand 1))
        bunch-o-pts (map pt/new-pt (take n (repeatedly rand-fn)) (take n (repeatedly rand-fn)))]
    (shuffle (concat default-ch bunch-o-pts))))

(deftest march
  (is (cyclic-equal? default-ch
                     (jarvis-march (default-pts 100)))))

(deftest scan
  (is (cyclic-equal? default-ch
                     (grahams-scan (default-pts 100)))))

(deftest alg
  (is (cyclic-equal? default-ch
                     (chans-algorithm (default-pts 100)))))

(deftest all-equal
  (is (every? true? (for [a1 [jarvis-march grahams-scan chans-algorithm]
                         a2 [jarvis-march grahams-scan chans-algorithm]]
                     (cyclic-equal? (a1 (default-pts 100))
                                    (a2 (default-pts 100)))))))