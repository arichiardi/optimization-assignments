(ns knapsack.branchbound
  (:refer-clojure :exclude [partition])
  (:require [clojure.zip :as z]
            [clojure.core.reducers :as r]
            [clojure.pprint :as pp :refer [pprint]]
            [clojure.tools.trace :refer [trace]]
            [taoensso.timbre.profiling :refer [p]]
            [rhizome.viz :as viz])
  (:import java.util.BitSet))

(set! *warn-on-reflection* true)

;; For testing lazy seqs.
(def ^{:private true} realized-cells (atom 0))
(def ^{:private true} reset-cells #(reset! realized-cells 0))

;;;;;;;;;;;;;;;;
;; Relaxation ;;
;;;;;;;;;;;;;;;;
(defn- ^{:author "Andrea Richiardi"}
  relax
  "Computes the Branch and Bound relaxation for an item."
  [item]
  )

(defn- ^{:author "Andrea Richiardi"}
  value-over-weight
  "Retrieves the value/weight ratio."
  [item]
  (/ (first item) (second item)))

(defn- ^{:author "Andrea Richiardi"}
  estimate
  "Computes an optimistic value for the bonding phase. Uses linear relaxation.
  If the integer index excluded-idx is 0 < excluded-idx < (count
  items) then that item will be excluded from the estimate."
  [capacity sorted-indexed-items current-item-index ^BitSet solution-index-set]
  (second (reduce (fn [[k acc-val] [idx [iv iw]]]
                    (if (>= k 0)
                      (if-not (.get solution-index-set idx)
                        (if (> idx current-item-index)
                          (if (>= (- k iw) 0)
                            [(- k iw) (+ acc-val iv)]
                            [0 (+ acc-val (long (with-precision 10 :rounding FLOOR (* iv (/ k iw)))))])
                          [k acc-val])
                        [(- k iw) (+ acc-val iv)])
                      [k acc-val])) [capacity 0] sorted-indexed-items)))

;;;;;;;;;;
;; Tree ;;
;;;;;;;;;;
(defrecord Node [capacity estimate value children solution]
  java.lang.Object
  (toString [_]
    (str (apply str (interpose "\n" (map str [value capacity estimate]))) "\n" (some-> solution .toString))))

(defn- ^{:author "Andrea Richia"}
  node-ctor
  "Builds a node. Destructures the input in the following keys: capacity
  estimate value solution and children."
  [& {:keys [capacity estimate value children solution]
      :or {capacity 0 estimate 0 value 0 children nil solution nil}}]
  (Node. capacity estimate value children solution))

(defn- ^{:author "Andrea Richiardi"}
  node-with-children
  "Copy constructor of a node. Takes all the fileds from an existing
  node but (lazily) attaches the children to it."
  [node children]
  (let [{:keys [capacity estimate value _ solution]} node]
    (Node. capacity estimate value (lazy-seq children) solution)))

(defn- ^{:author "Andrea Richiardi"}
  node-with-estimate
  "Copy constructor of a node. Takes all the fileds from the existing
  node but attaches the input estimate to it."
  [node estimate]
  (let [{:keys [capacity _ value children solution]} node]
    (Node. capacity estimate value children solution)))

(defn- ^{:author "Andrea Richiardi"}
  children-ctor
  "A branch is always a Node with contains either the fact that we take
  the item or that we don't put it in the knapsack."
  [^Node taken-branch ^Node not-taken-branch]
  (lazy-seq [taken-branch not-taken-branch]))

(defn- ^{:author "Andrea Richiardi"}
  node-taken
  [node]
  (first (:children node)))

(defn- ^{:author "Andrea Richiardi"}
  node-not-taken
  [node]
  (first (next (:children node))))

(defn- ^{:author "Andrea Richiardi"}
  node-children
  [node]
  (:children node))

(defn- ^{:author "Andrea Richiardi"}
  node-branch?
  "Does it have children?"
  [node]
  (instance? Node node))

(defn- ^{:author "Andrea Richiardi"}
  add-solution-index
  "Returns a new BitSet with new-index added to it."
  [^BitSet solution ^long new-index]
  (doto solution (.set new-index)))

(defn- ^{:author "Andrea Richiardi"}
  bb-generate-branches
  "Generates a new pair of branches (taken and the not-taken branch) lazily."
  [estimate-f item-value item-weight item-index depth-1-node]
  (lazy-seq
   (let [{:keys [capacity estimate value solution children]} depth-1-node]
     (swap! realized-cells inc)
     (let [taken-solution (add-solution-index (if-let [^BitSet s solution]
                                                (.clone s)
                                                (BitSet.)) item-index)
           taken (node-ctor :capacity (- capacity item-weight)
                            :value (+ value item-value)
                            :solution taken-solution
                            :estimate (estimate-f item-index taken-solution))
           not-taken (node-with-estimate depth-1-node (estimate-f item-index solution))]
       (children-ctor taken not-taken)))))

(defn- ^{:author "Andrea Richiardi"}
  part-iter
  ([n coll i part parts]
   (if (seq coll)
     (if (< i n)
       (recur n (rest coll) (inc i) (cons (first coll) part) parts)
       (recur n coll 0 () (cons (reverse part) parts)))
     (reverse (if (seq part)
                (cons (reverse part) parts)
                parts))))
  ([n step coll i part parts]
   ("TODO")))

(defn- ^{:author "Andrea Richiardi"}
  partition
  ([n coll] (lazy-seq (part-iter n coll 0 () ())))
  ([n step coll] (lazy-seq (part-iter n step coll 0 () ())))
  ;; ([n step pad coll]
    ;; (lazy-seq (partition n coll 0 () ())))

  )

(defn- ^{:author "Andrea Richiardi"}
  depth-monoid-op
  [acc-depth-1-pairs depth-pairs]
  ;; We need to use (into [] (r/take 2..) ...) or our custom partition here because (partition 2
  ;; ...) produces StackOverflow
  (partition 2 (map #(node-with-children %1 %2) (flatten depth-pairs) acc-depth-1-pairs)))

(defn- ^{:author "Andrea Richiardi"}
  bb-tree
  "Builds the tree, returning its root."
  ([estimate-f capacity indexed-items]
   (let [root (node-ctor :capacity capacity :solution (BitSet.))]
     (bb-tree estimate-f indexed-items root (list root) nil)))
  ([estimate-f indexed-items root depth-1 acc]
   (if (seq indexed-items)
     (let [[index [value weight]] (first indexed-items)
           new-depth (map #(bb-generate-branches estimate-f value weight index %1) (trace :depth-1 (flatten depth-1)))]
       (recur estimate-f (rest indexed-items) root new-depth (cons new-depth acc)))
     (node-with-children root (flatten (reduce depth-monoid-op (first acc) (rest acc)))))))

(def repl-args "-f src/knapsack/data/ks_lecture_dp_2")
(def input {:item-count 3, :capacity 10, :items [[45 5] [48 8] [35 3]]})
;; (def input (knapsack.solver/generate-input repl-args))

(defn- show-tree
  [root]
  (viz/view-tree node-branch? node-children root
                 :options {:dpi 56}
                 :node->descriptor (fn [^Node n] {:label (.toString n)})))

(defn ^{:author "Andrea Richiardi"}
  solve-bb-iterative
  [input]
  (reset-cells)
  (let [{:keys [item-count ^long capacity items] :as input-map} input
        indexed-items (map-indexed #(vector %1 %2) items)
        sorted-indexed-items (trace :sorted (sort-by #(value-over-weight (second %1)) > indexed-items))
        estimate-f (partial estimate capacity sorted-indexed-items)
        tree (bb-tree estimate-f capacity indexed-items)
        ]
    tree
    #_{:opt true
     :obj objective
     :taken-items (map-indexed (fn [index item] (.get taken-indexes index)) items)}))
