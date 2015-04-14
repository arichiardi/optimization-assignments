(ns ^{:author "Andrea Richiardi"
      :doc "Branch and bound implementation targeted at problems with small to medium number of
items (solution 1, ks_400_0 max) and over (solution 2, ks_10000_0).
It produces the solution tree and it then walks in depth using best-search first." }
  knapsack.branchbound.impl
  (:require [clojure.zip :as z]
            [clojure.core.reducers :as r]
            [clojure.pprint :as pp :refer [pprint]]
            [clojure.tools.trace :refer [trace]]
            [clojure.test :as t]
            [taoensso.timbre.profiling :refer [p]]
            [knapsack.branchbound :as bb])
  (:import java.util.BitSet))

(set! *warn-on-reflection* true)

;;;;;;;;;;;;;;;;
;; Solution 1 ;;
;;;;;;;;;;;;;;;;

;; For testing lazy seqs.
(def ^{:private true} realized-cells (atom 0))
(def ^{:private true} reset-cells #(reset! realized-cells 0))

(defn- bb-generate-branches
  "Generates a new pair of branches (taken and the not-taken branch) lazily."
  [estimate-f item-value item-weight item-index depth-1-node]
  (lazy-seq
   (let [{:keys [capacity estimate value solution children]} depth-1-node]
     (swap! realized-cells inc)
     (let [taken-solution (bb/add-solution-index (if-let [^BitSet s solution]
                                                   (.clone s)
                                                   (BitSet.)) item-index)
           taken (bb/node-ctor :capacity (- capacity item-weight)
                               :value (+ value item-value)
                               :solution taken-solution
                               :estimate (estimate-f item-index taken-solution)
                               :index item-index)
           not-taken (bb/node-ctor :capacity capacity
                                   :value value
                                   :solution solution
                                   :estimate (estimate-f item-index solution)
                                   :index item-index)]
       (bb/children-ctor taken not-taken)))))

(defn- depth-monoid-op
  [acc-depth-1-pairs depth-pairs]
  (partition 2 (map #(bb/node-with-children %1 (filter bb/node-valid? %2))
                    (flatten depth-pairs) acc-depth-1-pairs)))

(defn- bb-bottom-up-tree
  "Builds the tree, returning its root."
  ([estimate-f capacity indexed-items]
   (let [solution (BitSet.)
         root (bb/node-ctor :capacity capacity
                            :estimate (estimate-f -1 solution)
                            :solution solution
                            :index -1)]
     (bb-bottom-up-tree estimate-f indexed-items root (list root) nil)))
  ([estimate-f indexed-items root depth-1 acc]
   (if (seq indexed-items)
     (let [[index [value weight]] (first indexed-items)
           new-depth (map #(bb-generate-branches estimate-f value weight index %1) (flatten depth-1))]
       (recur estimate-f (rest indexed-items) root new-depth (cons new-depth acc)))
     (bb/node-with-children root (flatten (reduce depth-monoid-op (first acc) (rest acc)))))))


(defn ^{:author "Andrea Richiardi"}
  solve-bb-best-first-bottom-up-lazy
  "It lazily produces the solution tree and it then walks in depth using
  best-search first."
  [input]
  (reset-cells)
  (let [{:keys [item-count ^long capacity items] :as input-map} input
        indexed-items (map-indexed vector items)
        sorted-indexed-items (sort-by #(bb/value-over-weight (second %1)) > indexed-items)
        estimate-f (partial bb/fractional-estimate capacity sorted-indexed-items)
        tree (bb-bottom-up-tree estimate-f capacity indexed-items)
        best-node ^Node (first (pprint (bb/best-first-seq :estimate
                                                             bb/node-branch? bb/node-children tree)))]
    {:opt true
     :obj (:value best-node)
     :taken-items (map-indexed (fn [index item] (.get ^BitSet (:solution best-node) index)) items)}))

(defn- lazy-bottom-up-tree
  "Using the same building algorhitm of the previous solution, just produces the tree."
  [input]
  (reset-cells)
  (let [{:keys [item-count ^long capacity items] :as input-map} input
        indexed-items (map-indexed vector items)
        sorted-indexed-items (sort-by #(bb/value-over-weight (second %1)) > indexed-items)
        estimate-f (partial bb/fractional-estimate capacity sorted-indexed-items)]
    (bb-bottom-up-tree estimate-f capacity indexed-items)))

;;;;;;;;;;;;;;;;
;; Solution 2 ;;
;;;;;;;;;;;;;;;;

(defn- bb-make-children
  "Generates the children of the input depth-node."
  [estimate-f ^long item-count items ^knapsack.branchbound.Node depth-node]
  (swap! realized-cells inc)
  (let [{:keys [capacity estimate value solution children]} depth-node
        depth+1-index (inc (:index depth-node))
        [item+1-value item+1-weight] (nth items depth+1-index nil)
        new-capacity (- capacity item+1-weight)
        not-taken (bb/node-ctor :capacity capacity
                                :value value
                                :solution solution
                                :estimate (estimate-f depth+1-index solution)
                                :index depth+1-index)]
    (if (>= new-capacity 0)
      (let [new-solution (bb/add-solution-index (if-let [^BitSet s solution]
                                                  (.clone s)
                                                  (BitSet.)) depth+1-index)
            taken (bb/node-ctor :capacity new-capacity
                                :value (+ value item+1-value)
                                :solution new-solution
                                :estimate (estimate-f depth+1-index new-solution)
                                :index depth+1-index)]
        (bb/children-ctor taken not-taken))
      (bb/child-ctor not-taken))))

;; (def repl-args "-f src/knapsack/data/ks_lecture_dp_2")
;; (def input {:item-count 3, :capacity 10, :items [[45 5] [48 8] [35 3]]})
;; (def input (knapsack.solver/generate-input repl-args))
;; (def output (solve-bb-best-first-top-down input))
;; (knapsack.solver/assert-solution-capacity input output)

(defn ^{:author "Andrea Richiardi"}
  solve-bb-best-first-top-down
  "It produces the solution tree top down while walking in depth using best-search first.
  This is completely different from the previous version who was
  generating the tree (though lazily) beforehand. This solution is the
  best option for problem with a large number of items as it should,
  given a good euristic, visit the minimal set of nodes in the tree,
  avoiding going too deep while searching."
  [input]
  (reset-cells)
  (let [{:keys [item-count ^long capacity items] :as input-map} input
        sorted-items (sort-by bb/value-over-weight > items)
        sorted-indexed-items (map-indexed vector sorted-items)
        item->sorted-index (->> (apply merge (map (comp (partial apply hash-map)
                                                    (fn [[index item]] (vector item index)))  
                                              sorted-indexed-items)))
        estimate-f (partial bb/fractional-estimate capacity sorted-indexed-items)
        children-f (partial bb-make-children estimate-f item-count sorted-items)
        solution (BitSet.)
        root (bb/node-ctor :capacity capacity
                           :estimate (estimate-f -1 solution)
                           :solution solution
                           :index -1)
        best-node (bb/best-first-node :estimate
                                      (fn [node] (not= (:index node) (dec item-count)))
                                      children-f
                                      root)]
    {:opt true
       :obj (:value best-node)
     :taken-items (map #(.get ^BitSet (:solution best-node) (get item->sorted-index %1)) items)}))


(t/deftest test-best-first-top-down

  (t/testing "Corner cases"
    (def input {:item-count 0, :capacity 10, :items []})
    (t/is (= 0 (:obj (solve-bb-best-first-top-down input))))
    (t/is (= () (:taken-items (solve-bb-best-first-top-down input))))
    
    (def input {:item-count 1, :capacity 10, :items [[42 11]]})
    (t/is (= 0 (:obj (solve-bb-best-first-top-down input))))
    (t/is (= '(false) (:taken-items (solve-bb-best-first-top-down input))))

    (def input {:item-count 1, :capacity 10, :items [[42 10]]})
    (t/is (= 42 (:obj (solve-bb-best-first-top-down input))))
    (t/is (= '(true) (:taken-items (solve-bb-best-first-top-down input))))

    (def input {:item-count 2, :capacity 10, :items [[42 11] [8 10]]})
    (t/is (= 8 (:obj (solve-bb-best-first-top-down input))))
    (t/is (= '(false true) (:taken-items (solve-bb-best-first-top-down input))))

    (def input {:item-count 5, :capacity 10, :items [[42 11] [43 11] [44 11] [45 11] [8 10]]})
    (t/is (= 8 (:obj (solve-bb-best-first-top-down input))))
    (t/is (= '(false false false false true) (:taken-items (solve-bb-best-first-top-down input))))

    (def input {:item-count 3, :capacity 10, :items [[1 2] [15 5] [6 3]]})
    (t/is (= 22 (:obj (solve-bb-best-first-top-down input))))
    (t/is (= '(true true true) (:taken-items (solve-bb-best-first-top-down input))))))
