(ns convex-hull.jarvis-march
  (:require [geom.pt :as pt]
            [geom.seg :as seg]))

(defn- binary-search
  "A specialized binary search to find the peak of a unimodal distribution.
   Returns a map consisting of the element and the index in the vector
   of the element."
  ([coll comp]
     (binary-search (vec coll) comp 0 (dec (count coll))))
  ([coll comp start end]
     (let [len (- end start)
           half-index (int (/ (+ start end) 2))
           index-to-compare (dec half-index)]
       (if (= len 0)
         {:element (nth coll start) :index start}
         (if (= len 1)
           (if (< 0 (comp (nth coll end) (nth coll start)))
             {:element (nth coll start) :index start}
             {:element (nth coll end) :index end})
           (if (< 0 (comp (nth coll half-index) (nth coll index-to-compare)))
             (recur coll comp start half-index)
             (recur coll comp half-index end)))))))

(defn- angle-between-seg-and-pt [s p]
  (if (or (= p (s :e2))
          (= p (s :e1)))
    Math/PI
    (seg/angle-between-segs s (seg/new-seg (s :e2) p))))

(defn- part-of-prev-seg? [prev-seg p]
  (or (= (prev-seg :e2) p)
      (= (prev-seg :e1) p)))

(defn- next-pt
  ([pts prev-seg]
      (let [segs (->> pts
                      (remove (partial part-of-prev-seg? prev-seg))
                      (map #(seg/new-seg (prev-seg :e2) %)))
            angles (map #(seg/angle-between-segs prev-seg %) segs)
            s-a-map (zipmap segs angles)
            [best-seg best-angle] (apply min-key val s-a-map)]
        (best-seg :e2)))
  ([pts prev-seg convex-hulls]
     (if (empty? convex-hulls)
       (next-pt pts prev-seg)
       (let [compare-angles-dir (fn [dir]
                                  (comparator (fn [p1 p2]
                                                (dir (angle-between-seg-and-pt prev-seg p1)
                                                     (angle-between-seg-and-pt prev-seg p2)))))
             compare-angles-up (compare-angles-dir <)
             compare-angles-down (compare-angles-dir >)
             ;; Go up the hills
             best-pts-1 (map (comp :element #(binary-search % compare-angles-up)) convex-hulls)
             ;; Split at the bottom of the hills
             split-convex-hulls (remove empty? (mapcat (fn [hull] (split-at (:index (binary-search hull compare-angles-down)) hull)) convex-hulls))
             ;; Go up the split hills
             best-pts-2 (map (comp :element #(binary-search % compare-angles-up)) split-convex-hulls)
             best-pts (concat best-pts-1 best-pts-2)]
         ;; Find the best of the best
         (next-pt best-pts prev-seg)))))

(defn jarvis-march
  "Finds the convex hull of a set of points by
   the algorithm known as the 'Jarvis March'."
  [pts & convex-hulls]
  (let [bottom-pt (apply min-key :y pts)
        dummy-seg (seg/new-seg (pt/new-pt (dec (bottom-pt :x)) (bottom-pt :y)) bottom-pt)
        lazy-j-m (fn lazy-j-m [prev-seg]
                   (let [nxt-pt (next-pt pts prev-seg convex-hulls)]
                     (if (= nxt-pt bottom-pt)
                       nil
                       (lazy-seq (cons nxt-pt (lazy-j-m (seg/new-seg (prev-seg :e2) nxt-pt)))))))]
    (lazy-cat (lazy-j-m dummy-seg) (list bottom-pt))))
