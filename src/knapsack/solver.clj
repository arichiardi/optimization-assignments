(ns ^{:author "Andrea Richiardi"
      :doc "My attempt to the knapsack problem, in Clojure."}
  knapsack.solver
  (:require [clojure.string :as string :refer [trim blank? split join]]
            [clojure.java.io :as io :refer [reader]]
            [clojure.pprint :as pp :refer [pprint]]
            [clojure.tools.cli :refer [parse-opts summarize]]
            [clojure.data.int-map :refer [int-map update!]]
            [clojure.tools.trace :refer [trace]]
            [taoensso.timbre.profiling :refer [p]]
            [assignments.core :refer [solver-main
                                      benchmark-main
                                      *solve-fn*
                                      *create-output-fn*
                                      *parse-file-fn*]])
  (:gen-class))

(set! *warn-on-reflection* true)

(declare parse-file)
(declare solve-greedy)
(declare solve-dp-iterative)
(declare output-string)

(def repl-args "-f src/knapsack/data/ks_19_0")
(def generate-input #(parse-file (second (split repl-args #"[ ]"))))

(defn -main
  "Solves the problem at hand and prints the result on stdout."
  [& args]
  (binding [*parse-file-fn* parse-file
            *solve-fn* solve-dp-iterative
            *create-output-fn* output-string]
    (apply solver-main args)))

(defn benchmark
  [& args]
  (binding [*parse-file-fn* parse-file
            *solve-fn* solve-dp-iterative]
    (apply benchmark-main args)))

(defn parse-lines
  "Recursive parsing function accepting lines in the format v<space>w and
  returning a vector of [v w] vectors"
  ([line-seq max-lines] (parse-lines line-seq max-lines 0 []))
  ([line-seq max-lines current-line result]
   (if (and (seq line-seq) (< current-line max-lines))
     (let [[vx wx] (split (first line-seq) #"[\s\t]")]
       (recur (rest line-seq) max-lines (inc current-line) (conj result (vector (Long/parseLong vx) (Long/parseLong wx)))))
     result)))

(defn- parse-file
  "Returns the following input map:
  {:item-count n, :capacity K,
   :items [[v_0 w_0] [v_1 w_1] ... [v_n-1 w_n-1]]}"
  [file-name]
  (with-open [rdr (reader file-name)]
    (let [lines (line-seq rdr)
          first-line (split (first lines) #"[\s\t]")
          item-count (Long/parseLong (first first-line))
          capacity (Long/parseLong (second first-line))
          rest-of-lines (rest lines)]
      {:item-count item-count
       :capacity capacity
       :items (parse-lines rest-of-lines item-count)})))

(defn- bool->long
  [value]
  (if value 1 0))

(defn- output-string
  "Parses the solution map and build the solution outputstring.
  The solution map should have the following format:
  {:obj x
   :opt [t or f]
   :taken-items [t f t f f f t ...]}."
  [solution]
  (let [{:keys [obj opt taken-items]} solution]
    (str obj " " (bool->long opt) "\n" (join " " (map bool->long taken-items)))))

;; A greedy performance disaster
(defn- solve-greedy-iter
  ([items capacity] (solve-greedy-iter items capacity 0 0 []))
  ([items capacity cum-value cum-weight cum-result]
   (if-let [is (seq items)]
     (let [[value weight] (first is)]
       (if (<= (+ cum-weight weight) capacity)
         (recur (rest is) capacity (+ value cum-value) (+ weight cum-weight) (conj cum-result true))
         (recur (rest is) capacity cum-value cum-weight (conj cum-result false))))
     {:obj cum-value :taken-items cum-result})))

(defn- ^{ :author "Coursera Team" } solve-greedy
  [input]
  (let [{:keys [item-count capacity items] :as input-map} input]
    (assoc (solve-greedy-iter items capacity) :opt false)))



(defn transient-int-memoize
  "Returns a memoized version of a referentially transparent function
  that accepts a single integer argument in the range [0,
  Long/MAX_VALUE]. The returned function keeps a int-map as cache and is
  more efficient than the default memoize for this particular set of
  functions. This function uses a transient and it is not thread safe."
  [f]
  (let [cache (atom (transient (int-map)))]
    (fn [key]
      (if-let [e (find @cache key)]
        (val e)
        (let [new-val (f key)]
          (swap! cache update! key (fn [_] new-val))
          new-val)))))

(defn int-memoize
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dynamic programming (recursive solution, iterative process)
;;   The following solution uses an hash-table of lazy lists of values for backing up the bottom-up
;;   lookup table for the knapsack problem. The outer loop is on the capacity (0..K) whereas the
;;   inner is on the items. This solution works very well for small search spaces as it is always
;;   optimal, but fails with OutOfMemoryError when the number of items/capacity grows.

(defn- ^{:author "Andrea Richiardi"}
  compute-cell
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
     (let [i+1-value (p :compute-cell (compute-cell k ith-value table (first is) i))]
       (recur (rest items) table k i+1-value (inc i) (cons i+1-value partials)))
     (reverse partials))))

(defn- ^{ :author "Andrea Richiardi" } dp-iter-on-capacity
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

(defn- ^{ :author "Andrea Richiardi" } solve-dp-naive-table-iterative
  [input]
  "A solver using dynamic programming and iterative processing. Small k is the current capacity. The
  solution is built with the optimized clojure.data.int-map/int-map and its mapped values are all
  lazy in order to avoid unnecessary computations."
  (let [{:keys [item-count capacity items] :as input-map} input]
    ;; (reset! realized-cells 0)
    (let [table (dp-iter-on-capacity items capacity)]
      {:opt true
       :obj (nth (get table capacity) (count items))
       :taken-items (backtrack items capacity table)})))
