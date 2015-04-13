(ns ^{:author "Andrea Richiardi"
      :doc "Branch and bound implementation, common functions." }
  knapsack.branchbound
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

(defn value-over-weight
  "Retrieves the value/weight ratio."
  ^double [item]
  (/ (double (first item)) (second item)))

(defn sum-helper
  "Helper for summing and computing the optimistic evaluation."
  [processed-index ^BitSet solution sorted-indexed-items acc]
  (let [[k acc-sum] acc]
    (if (and (seq sorted-indexed-items) (> k 0))
      (let [[idx [iv iw]] (first sorted-indexed-items)] 
        (if-not (.get solution idx)
          (if (> idx processed-index)
            (if (>= (- k iw) 0)
              (recur processed-index solution (rest sorted-indexed-items) [(- k iw) (+ acc-sum iv)])
              (recur processed-index solution (rest sorted-indexed-items) [0 (+ acc-sum (long (* iv (/ (double k) iw))))]))
            (recur processed-index solution (rest sorted-indexed-items) [k acc-sum]))
          (recur processed-index solution (rest sorted-indexed-items) [(- k iw) (+ acc-sum iv)])))
      acc)))

(defn sum-all-estimate
  "Computes an optimistic value for the bonding phase. Uses linear relaxation.
  If the integer index excluded-idx is 0 < excluded-idx < (count
  items) then that item will be excluded from the estimate [Just for DEBUG]."
  ^long [capacity sorted-indexed-items item-index ^BitSet solution-index-set]
  (second (r/reduce (fn [[k acc-val] [idx [iv iw]]]
                      (if (> k 0)
                        (if-not (.get solution-index-set idx)
                          (if (> idx item-index)
                            [(- k iw) (+ acc-val iv)]
                            [k acc-val])
                          [(- k iw) (+ acc-val iv)])
                        [k acc-val])) [capacity 0] sorted-indexed-items)))


(defn fractional-estimate
  "Computes an optimistic value for the bonding phase. Uses linear relaxation.
  If the integer index excluded-idx is 0 < excluded-idx < (count items)
  then that item will be excluded from the estimate."
  ^long [^long capacity sorted-indexed-items processed-index ^BitSet solution]
  (second (sum-helper processed-index solution sorted-indexed-items [capacity 0])))

;;;;;;;;;;
;; Node ;;
;;;;;;;;;;

(defrecord Node [capacity estimate value index children solution]
  java.lang.Object
  (toString [_]
    (str (clojure.string/join "\n" (map str [value capacity estimate])) "\n" (some-> solution .toString))))

(defn node-horiz-str
  [{:keys [capacity estimate value index children solution]}]
  (str "|" (clojure.string/join " " [index value capacity estimate (some-> solution .toString)]) "|"))

(defn node-ctor
  "Builds a node. Destructures the input in the following keys: capacity
  estimate value solution and children."
  [& {:keys [capacity estimate value index children solution]
      :or {capacity 0 estimate 0 value 0 index -1 children nil solution nil}}]
  (Node. capacity estimate value index children solution))

(defn node-with-children
  "Copy constructor of a node. Takes all the fileds from an existing
  node but (lazily) attaches the children to it."
  [^Node node children]
  (let [{:keys [capacity estimate value index _ solution]} node]
    (Node. capacity estimate value index (lazy-seq children) solution)))

(defn node-with-estimate
  "Copy constructor of a node. Takes all the fileds from the existing
  node but attaches the input estimate to it."
  [^Node node ^long estimate]
  (let [{:keys [capacity _ value index children solution]} node]
    (Node. capacity estimate value index children solution)))

(defn children-ctor
  "A branch is always a Node with contains either the fact that we take
  the item or that we don't put it in the knapsack."
  [^Node taken-branch ^Node not-taken-branch]
  (lazy-seq (remove nil? [taken-branch not-taken-branch])))

(defn child-ctor
  "A branch is always a Node with contains either the fact that we take
  the item or that we don't put it in the knapsack."
  [^Node not-taken-branch]
  (lazy-seq [not-taken-branch]))

(defn node-taken
  [^Node node]
  (first (:children node)))

(defn node-not-taken
  [^Node node]
  (fnext (:children node)))

(defn node-children
  [^Node node]
  (:children node))

(defn node-children
  [^Node node]
  (:children node))

(defn node-has-children?
  [^Node node]
  (seq (:children node)))

(def ^{:doc "Is it a branch? In this representation if it has children it is a branch, otherwise a
leaf"} node-branch? node-has-children?)

(defn node-valid?
  [^Node node]
  (>= (:capacity node) 0))

(defn ^{ :author "Andrea Richiardi" }
  assert-node-solution-capacity
  "Calculates the total capacity of a solution."
  [{:keys [item-count capacity items] :as input-map}
   {:keys [^BitSet solution] :as node}]
  (let [output-capacity (reduce (fn [cum [idx [iv iw]]] (if (.get solution idx)
                                                         (+ cum iw)
                                                         cum))
                                0
                                (map-indexed vector items))] 
    (println "Assert " capacity " >= " output-capacity) 
    (assert (>= capacity output-capacity))))

(defn add-solution-index
  "Returns a new BitSet with new-index added to it."
  [^BitSet solution ^long new-index]
  (doto solution (.set new-index)))

(defn show-tree
  [root]
  (viz/view-tree node-branch? node-children root
                 :options {:dpi 48}
                 :node->descriptor (fn [n] {:label (str n)})
                 :node->cluster :index
                 :cluster->descriptor (fn [n] {:label n})))

(defn dump-tree
  [root filename]
  (viz/save-tree node-branch? node-children root
                 :filename filename
                 :options {:dpi 24}
                 :node->descriptor (fn [n] {:label (str n)})
                 :node->cluster :index
                 :cluster->descriptor (fn [n] {:label n})))

(defn- best-first-acc-walk
  [keyfn branch? children best-nodes unvisited]
  (let [best (first best-nodes)]
    (if (branch? best) 
      (let [sorted (sort-by keyfn > (concat unvisited (seq (children best))))
            new-best (first sorted)]
        (recur keyfn branch? children (cons new-best best-nodes) (rest sorted)))
      (do (assert (empty? (filter #(and (branch? %1) (> (:estimate %1) (:estimate best))) unvisited))) 
        #_(doall (map #(println (node-horiz-str %1)) unvisited))
        best-nodes))))

(defn best-first-seq
  "It explores the tree using best-first search. Returns a lazy list of
  nodes which are valued the best according to keyfn, a function
  accepting a node and returning the value to compare on.
  The other parameters follow tree-seq convention: branch? evaluates
  true when the input node can have children (even if it does not have
  them) and children returns them given a node."
  [keyfn branch? children root]
  {:pre [(some? root)]}
  (lazy-seq (best-first-acc-walk keyfn branch? children (list root) nil)))

(defn- best-first-simple-walk
  [keyfn branch? children best-node unvisited]
  (if (branch? best-node)
    (let [sorted (sort-by keyfn > (concat unvisited (seq (children best-node))))
          new-best (first sorted)]
      (if new-best 
        (recur keyfn branch? children new-best (rest sorted)) 
        best-node))
    (do (assert (empty? (filter #(and (branch? %1) (> (:estimate %1) (:estimate best-node))) unvisited))) 
        #_(doall (map #(println (node-horiz-str %1)) unvisited))
        best-node)))

(defn best-first-node
  "It explores the tree using best-first search. Returns a lazy list of
  nodes which are valued the best according to keyfn, a function
  accepting a node and returning the value to compare on.
  The other parameters follow tree-seq convention: branch? evaluates
  true when the input node can have children (even if it does not have
  them) and children returns them given a node."
  [keyfn branch? children root]
  {:pre [(some? root)]}
  (best-first-simple-walk keyfn branch? children root nil))
