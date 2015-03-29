(ns knapsack.dp
  (:require [clojure.core.reducers :as r]
            [clojure.pprint :as pp :refer [pprint]]
            [clojure.data.int-map :as di :refer [int-map int-set]]
            [clojure.tools.trace :refer [trace]]
            [taoensso.timbre.profiling :refer [p]]))

;; Some fun with memoization
(defn- int-memoize
  "Returns a memoized version of a referentially transparent function
  that accepts a single integer argument in the range [0,
  Long/MAX_VALUE]. The returned function keeps a int-map as cache and is
  more efficient than the default memoize for this particular set of
  functions. This function uses a transient and it is not thread safe."
  [f]
  (let [cache (atom (int-map))]
    (fn [key]
      (if-let [e (find @cache key)]
        (val e)
        (let [new-val (f key)]
          (swap! cache assoc key new-val)
          new-val)))))

;; For testing lazy seqs.
;; (def ^{:private true} realized-cells (atom 0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dynamic programming (recursive solution, iterative process)
;;   The following solution uses an hash-table of lazy lists of values for backing up the bottom-up
;;   lookup table for the knapsack problem. The outer loop is on the capacity (0..K) whereas the
;;   inner is on the items. This solution works very well for small search spaces as it is always
;;   optimal, but fails with OutOfMemoryError when the number of items/capacity grows.

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
  dp-iter-k-partials
  "Computes a lazy sequence of the dp solution for the k-th capacity
  step, maximizing the values of our objective function. Here k and i
  are indexes: k from 0 to capacity (outer loop) and i from 0 to (count
  items), controlled by this iterative process."
  ([items table k] (lazy-seq (dp-iter-k-partials items table k 0 1 (list 0))))
  ([items table k ith-value i partials]
   (if-let [is (seq items)]
     (let [i+1-value (p :compute-value (compute-value k ith-value table (first is) i))]
       (recur (rest items) table k i+1-value (inc i) (cons i+1-value partials)))
     (reverse partials))))

(defn- ^{ :author "Andrea Richiardi" }
  dp-iter-on-capacity
  ([items capacity] (dp-iter-on-capacity items capacity 0 {}))
  ([items capacity k cum-table]
   (if (<= k capacity)
     (recur items capacity (inc k) (assoc cum-table k (dp-iter-k-partials items cum-table k)))
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
  "A solver using dynamic programming and iterative processing. The solution is built with the
  optimized clojure.data.int-map/int-map and the mapped value lists are all lazy in order to avoid
  unnecessary computations. Still, this solution takes too much space."
  (let [{:keys [item-count capacity items] :as input-map} input]
    ;; (reset! realized-cells 0)
    (let [table (dp-iter-on-capacity items capacity)]
      {:opt true
       :obj (nth (get table capacity) (count items))
       :taken-items (backtrack items capacity table)})))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dynamic programming (recursive solution, iterative process)
;;   The following solution tries to reduce the memory footprint of the previous. The outer loop is
;;   on the items this time while the inner on the capacity (0..K). This allows to compute just
;;   one "columns" of values at the time and therefore should avoid OutOfMemoryErrors.
;;   This is still not true as the column, thought as a map k -> solution-so-far is still memory
;;   eager. A better representation can be a (int-set of ks) -> solution-so-far.

(defn dp-get-cell-by-k
  "Gets the cell given a capacity k"
  [column k]
  (get column k)
  ;; (some-> (first (into [] (r/filter #(not= 0 (count (di/intersection (val %1) (conj (di/int-set) k)))) column)))
          ;; key)
  ;; me-> (first (filter #(empty? di/intersection (val %1) #{k}) column))
  ;;         key))

  )

(defn- ^{:author "Andrea Richiardi"}
  dp-compute-cell
  "Computes a cell by loog at the maximum between the old value with
  the current capacity and (if the item fits) the item's value plus the
  value stored in the table for the difference between the item's weight
  and the input capacity."
  [prev-column item i k]
  (let [[value weight] item
        remaining-k (- k weight)
        ;; (item-index-1, k)
        prev-cell (dp-get-cell-by-k prev-column k)
        prev-value (or (first prev-cell) 0)]
    (if (<= weight k)
      ;; The item fits -> I need to check value at (item-index-1, k - weight)
      ;; but the current representation of prev-column does not allow me to get a cell by k.
      ;; I need to iterate on all its (key, value) pairs.
      (let [alt-cell (dp-get-cell-by-k prev-column remaining-k)
            alt-value (+ value (or (some-> alt-cell first) 0))]
        (if (> alt-value prev-value)
          (vector alt-value (int-set (conj (second alt-cell) i))) ;; select the current item
          (vector prev-value (second prev-cell))))
      (vector prev-value (second prev-cell)))))

(defn- ^{:author "Andrea Richiardi" }
  dp-compute-column
  "A column is represented as a map from a cell to a range (a bit set)
  of capacities k associated with that cell. Partials is a map that
  caches the cell generated so far so that we save some memory in the
  progress."
  ([capacity useful-weights item i prev-column] (dp-compute-column capacity
                                                                   useful-weights
                                                                   item i prev-column
                                                                   (transient (di/int-map))
                                                                   (hash-set)))
  ([capacity useful-weights item i prev-column new-column partials]
   (if (seq useful-weights)
     (let [k (first useful-weights)
           cell (dp-compute-cell prev-column item i k)
           ;; simply caches cells
           [cached-cell partials] (if (contains? partials cell) [cell partials] [cell (conj partials cell)])]

       (recur capacity (rest useful-weights) item i prev-column (assoc! new-column k cached-cell) partials
              ;; (if-let [value (get new-column cell)] ;; value is an int-set here
                ;; (assoc! new-column cell (di/union value (conj (di/int-set) k)))
              ;; (assoc! new-column cell (conj (di/int-set) k)))
       ))
     (persistent! new-column))))



(defn- ^{:author "Andrea Richiardi" }
  dp-compute-column-pairs
  ([capacity items useful-weights]
   (dp-compute-column-pairs capacity useful-weights (partition 2 (map-indexed #(vector (inc %1) %2) items)) (int-map)))
  ([capacity useful-weights item-pairs prev-column]
   (if (seq item-pairs)
     (recur capacity useful-weights (rest item-pairs) (let [item-pair (first item-pairs)
                                                            first-index (ffirst item-pair)
                                                            first-item (second (first item-pair))
                                                            second-index (first (second item-pair))
                                                            second-item (second (second item-pair))
                                                            first-column (dp-compute-column capacity useful-weights first-item first-index prev-column)]
                                                        (dp-compute-column capacity useful-weights second-item second-index first-column)))
     prev-column)))

(defn feasible-capacities
  "Computes the set of feasible capacities given the input set. This is
  useful when the capacity is a very high number to avoid computation."
  [capacity items]
  (into #{} (conj (flatten (for [outer items
                                 inner items
                                 :let [weight-outer (second outer)
                                       weight-inner (second inner)
                                       weight-sum (+ weight-outer weight-inner)]
                                 :when (or (not= outer inner) (<= weight-sum capacity))]
                             [weight-outer weight-inner weight-sum])) capacity)))

(def repl-args "-f src/knapsack/data/ks_82_0")

(defn ^{ :author "Andrea Richiardi" }
  solve-dp-memory-conscious-iterative
 "A solver using dynamic programming and iterative processing.  The
  following solution tries to reduce the memory footprint of the
  big-lookup-table one. The outer loop is on the items this time while
  the inner on the capacity (0..K). This allows to compute just one
  column of values at the time and therefore should avoid
  OutOfMemoryErrors. [LATER...] This is still not enough as the column,
  thought as a map k -> solution-so-far is still memory eager and
  contains a lot of duplication. A better representation can be
  a (int-set of ks) -> solution-so-far."
  [input]
  (let [{:keys [item-count capacity items] :as input-map} input
        useful-weights (trace :fw (feasible-capacities capacity items))
        ;; last-column nil
        last-column (dp-compute-column-pairs capacity items useful-weights) ;; compute
        precious (dp-get-cell-by-k last-column capacity) ;; take the optimal solution
        objective (first precious)
        taken-indexes (second precious)]
    {:opt true
     :obj objective
     :taken-items (map-indexed (fn [index item] (contains? taken-indexes (inc index))) items)}))
