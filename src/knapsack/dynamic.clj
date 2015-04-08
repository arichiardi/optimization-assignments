(ns knapsack.dynamic
  (:require [clojure.core.reducers :as r]
            [clojure.set :as set]
            [clojure.pprint :as pp :refer [pprint]]
            [clojure.tools.trace :refer [trace]]
            [taoensso.timbre.profiling :refer [p]])
  (:import java.util.BitSet))

(set! *warn-on-reflection* true)

;; For testing lazy seqs.
;; (def ^{:private true} realized-cells (atom 0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dynamic programming (recursive solution, iterative process)

(defn- ^{:author "Andrea Richiardi"}
  compute-value
  "Computes the value (a table cell) by looking at the maximum between
  the old value with the current capacity and (if the item fits) the
  item's value plus the value stored in the table for the difference
  between the item's weight and the input capacity."
  [capacity prev-value table item item-index]
  (let [[value weight] item
        remaining-capacity (- capacity weight)]
    ;; (swap! realized-cells inc)
    (if (<= weight capacity)
      (max prev-value ;; do not select the current item
           (+ value (or (some-> (get table remaining-capacity)
                                (nth (dec item-index) 0))
                        0)))
      prev-value)))

(defn- ^{:author "Andrea Richiardi"}
  dp-iter-on-items
  "Computes a lazy sequence of the dp solution for the k-th capacity
  step, maximizing the values of our objective function. Here k and i
  are indexes: k from 0 to capacity (outer loop) and i from 0 to (count
  items), controlled by this iterative process."
  ([items table k] (lazy-seq (dp-iter-on-items items table k 0 1 (list 0))))
  ([items table k ith-value i partials]
   (if-let [is (seq items)]
     (let [i+1-value (compute-value k ith-value table (first is) i)]
       (recur (rest items) table k i+1-value (inc i) (cons i+1-value partials)))
     (reverse partials))))

(defn- ^{ :author "Andrea Richiardi" }
  dp-iter-on-capacity
  ([items capacity] (dp-iter-on-capacity items capacity 0 {}))
  ([items capacity k cum-table]
   (if (<= k capacity)
     (recur items capacity (inc k) (assoc cum-table k (dp-iter-on-items items cum-table k)))
     cum-table)))

(defn- ^{:author "Andrea Richiardi"}
  backtrack
  "Backtracks the solution and computes the control variables. The var i
  is used in a countdown fashion. This function never check for missing
  rows/columns on the table (we want a crash in that case to quickly
  spot bugs)."
  ([items capacity table] (backtrack items capacity table
                                     (count items)
                                     (reverse items)
                                     (get table capacity)
                                     []))
  ([items capacity table i reverse-items ith-values taken-items]
   (if (> i 0)
     (let [[value weight] (first reverse-items)
           remaining-capacity (- capacity weight)
           ith-value (nth ith-values i)
           i-1th-value (nth ith-values (dec i))]
       (if (= ith-value i-1th-value)
         (recur items capacity table (dec i) (rest reverse-items) ith-values (conj taken-items false)) ;; was not taken
         (recur items remaining-capacity table (dec i) (rest reverse-items) (get table remaining-capacity) (conj taken-items true))))

     (rseq taken-items))))

(defn ^{ :author "Andrea Richiardi" } solve-dp-naive-table-iterative
  [input]
  "The following solution uses an hash-table (clojure.data.int-map/int-map) of lazy lists of values
   for backing up the bottom-up lookup table for the knapsack problem. The outer loop is on the
   capacity (0..K) whereas the inner is on the items. It works very well for small search spaces as
   it is always optimal, but fails with OutOfMemoryError when the number of items/capacity
   grows (from the lectures, it has a pseudo-polynomial comlexity)."
  (let [{:keys [item-count capacity items] :as input-map} input]
    ;; (reset! realized-cells 0)
    (let [table (dp-iter-on-capacity items capacity)]
      {:opt true
       :obj (nth (get table capacity) (count items))
       :taken-items (backtrack items capacity table)})))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dynamic programming (recursive solution, iterative process)

(defn ^{:author "Andrea Richiardi"}
  dp-get-cell-by-k
  "Gets the cell given a capacity k"
  [^"[Lclojure.lang.PersistentVector;" column ^long k]
  (aget column k))

(defn- ^{:author "Andrea Richiardi"}
  dp-compute-cell
  "Computes a cell by looking at the maximum between the old value with
  the current capacity and (if the item fits) the item's value plus the
  value stored in the table for the difference between the item's weight
  and the input capacity."
  [prev-column item ^long i ^long k]
  (let [[^long value ^long weight] item
        ;; (item-index-1, k)
        prev-cell (dp-get-cell-by-k prev-column k)
        ^long prev-value (or (first prev-cell) 0)]
    (if (<= weight k)
      ;; The item fits -> I need to check value at (item-index-1, k - weight)
      ;; but the current representation of prev-column does not allow me to get a cell by k.
      ;; I need to iterate on all its (key, value) pairs.
      (let [alt-cell (dp-get-cell-by-k prev-column (- k weight))
            ^long alt-value (+ value (or (some-> alt-cell first) 0))]
        (if (> alt-value prev-value)
          ;; selects the current item, adding in the bit set
          (vector alt-value (doto
                                (if-let [^BitSet bits (second alt-cell)] ^BitSet (.clone bits) (BitSet.))
                              (.set i)))
          (vector prev-value (second prev-cell))))
      (vector prev-value (second prev-cell)))))

(defn- ^{:author "Andrea Richiardi" }
  dp-compute-column
  "A column is represented as a map from a cell to a range (a bit set)
  of capacities k associated with that cell. Partials is a map that
  caches the cell generated so far so that we save some memory in the
  progress."
  ([capacity] (make-array clojure.lang.PersistentVector (inc capacity)))
  ([capacity item-k-map item i prev-column] (dp-compute-column capacity
                                                               (get item-k-map item)
                                                               item i prev-column
                                                               (dp-compute-column capacity)))
  ([capacity ks item i prev-column ^"[Lclojure.lang.PersistentVector;" new-column]
   (if (seq ks)
     (let [^long k (first ks)
           ^clojure.lang.PersistentVector cell (dp-compute-cell prev-column item i k)]
       ;; recurring with the new k from the map item->necessary ks
       (recur capacity (rest ks) item i prev-column (doto new-column (aset k cell))))
     new-column)))

(defn- ^{:author "Andrea Richiardi" }
  dp-compute-column-pairs
  "Computes column pairs, one column at the time in memory."
  ([capacity items item-k-map]
   (dp-compute-column-pairs capacity item-k-map
                            (partition-all 2 (map-indexed #(vector %1 %2) items))
                            (dp-compute-column capacity)))

  ([capacity item-k-map item-pairs prev-column]
   (if-let [pair-seq (seq item-pairs)]
     (recur capacity item-k-map (rest item-pairs) (let [item-pair (first pair-seq)
                                                        first-index (ffirst item-pair)
                                                        first-item (second (first item-pair))
                                                        second-index (first (second item-pair))
                                                        second-item (second (second item-pair))
                                                        first-column (dp-compute-column capacity item-k-map first-item first-index prev-column)]
                                                    (if (= (count item-pair) 2)
                                                      (dp-compute-column capacity item-k-map second-item second-index first-column)
                                                      first-column)))
     prev-column)))

(defn- ^{:author "Andrea Richiardi"}
  capacities-for-item-at-k
  "Gets the necessary ks given thecurrent capacity and an item."
  [^long k i]
  (let [^long i-weight (second i)]
    (if (< i-weight k) #{k (- k i-weight)} #{k})))

(defn ^{:author "Andrea Richiardi"}
  item->capacities
  "Computes the set of feasible capacities given the input set. This is
  useful when the capacity is a very high number to avoid
  computation. In English, the i-th item will need to have entries in
  the column for the i+1-th item. This function therefore will associate
  i-th item to the necessary k(s) for its next, starting from the last
  item, similarly to the backtrack algorithm of the naive
  implementation."
  ([capacity items]
   (item->capacities capacity (reverse items) (reverse (partition 2 1 items))
                     (transient (assoc (hash-map) (last items) #{capacity}))))
  ([capacity items outer-is item-k-map]
   (if (seq outer-is)
     (let [[i-1 i] (first outer-is)
           i-k-map (get item-k-map i)
           ;; Reducers!
           i-1-ks (r/fold set/union (r/map #(capacities-for-item-at-k %1 i) i-k-map))]
       (recur capacity (rest items) (rest outer-is) (assoc! item-k-map i-1 i-1-ks)))
     (persistent! item-k-map))))

;; (def repl-args "-f src/knapsack/data/ks_lecture_dp_2")

(defn ^{:author "Andrea Richiardi"}
  solve-dp-memory-conscious-iterative
  "A solver using dynamic programming and iterative processing.  The
  following solution tries to reduce the memory footprint of the
  big-lookup-table approach. The outer loop is on the items this time
  while the inner on the capacities (0..K). This allows to compute just
  one column of values at the time and therefore should avoid
  OutOfMemoryErrors. The solution then evolved to have a pre-walk in
  order to build the necessary ks for each item, this is done when the
  capacity/item ratio is > 5000 (empirical on Coursera data). Moreover,
  a java.util.BitSet now contains the item indexes of the solution."
  [input]
  (let [{:keys [item-count ^long capacity items] :as input-map} input
        item-k-map (if (or (> (/ capacity item-count) 5000) (>= item-count 10000)) ;; do I need a pre-walk?
                     (item->capacities capacity items)
                     ;; Reducers here are slower (benchmarked with 10000 elements) !
                     (apply merge (map #(hash-map %1 (range 1 (inc capacity))) items)))
        last-column (dp-compute-column-pairs capacity items item-k-map) ;; compute
        precious (dp-get-cell-by-k last-column capacity)                ;; take the optimal solution
        objective (first precious)
        taken-indexes ^BitSet (second precious)]
    {:opt true
     :obj objective
     :taken-items (map-indexed (fn [index item] (.get taken-indexes index)) items)}))
