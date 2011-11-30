(ns convex-hull.jarvis-march
  (:require [geom.pt :as pt]
            [geom.seg :as seg]))

(defn- next-pt [pts prev-seg]
  (let [segs (->> pts
                  (remove #(or (= (prev-seg :e2) %)
                               (= (prev-seg :e1) %)))
                  (map #(seg/new-seg (prev-seg :e2) %)))
        angles (map #(seg/angle-between-segs prev-seg %) segs)
        s-a-map (zipmap segs angles)
        [best-seg best-angle] (apply min-key val s-a-map)]
    (best-seg :e2)))

(defn jarvis-march
  "Finds the convex hull of a set of points by
   the algorithm known as the 'Jarvis March'."
  [pts]
  (let [bottom-pt (apply min-key :y pts)
        dummy-seg (seg/new-seg (pt/new-pt (dec (bottom-pt :x)) (bottom-pt :y)) bottom-pt)]
    (loop [ret [bottom-pt] prev-seg dummy-seg]
      (let [nxt-pt (next-pt pts prev-seg)]
        (if (= nxt-pt bottom-pt)
          ret
          (recur (conj ret nxt-pt) (seg/new-seg (prev-seg :e2) nxt-pt)))))))
