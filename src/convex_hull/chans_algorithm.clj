(ns convex-hull.chans-algorithm
  (:use [convex-hull.jarvis-march]
        [convex-hull.grahams-scan])
  (:require [geom.pt :as pt]
            [geom.seg :as seg]))

(defn chans-algorithm
  "Finds the convex hull of a set of points by
   the algorithm known as 'Chan's Algorithm'."
  [pts]
  (let [bottom-pt (apply min-key :y pts)]
   (loop [m 3] ; start out with triangles
     (let [partitioned-pts (partition m m [] pts)
           c-hs (map grahams-scan partitioned-pts)
           potential-ch (take m (apply jarvis-march pts c-hs))]
       (if (= bottom-pt (last potential-ch)) ; assumes jarvis-march returns bottom-pt last
         potential-ch
         (recur (min (* m m) (count pts))))))))



