(ns ^{:author "Andrea Richiardi"
      :doc "My attempt to the knapsack problem, in Clojure."}
  knapsack.solver
  (:require [clojure.string :as string :refer [trim blank? split join]]
            [clojure.java.io :as io :refer [reader]]
            [clojure.pprint :as pp :refer [pprint]]
            [clojure.tools.cli :refer [parse-opts summarize]]
            [assignments.core :refer [solver-main
                                      benchmark-main
                                      *solve-fn*
                                      *create-output-fn*
                                      *parse-file-fn*]]
            [assignments.common :refer [parse-lines
                                        output-string]]
            [knapsack.dynamic :as dp]
            [knapsack.branchbound.impl :as bl])
  (:gen-class))

(set! *warn-on-reflection* true)

(defn -main
  "Solves the problem at hand and prints the result on stdout."
  [& args]
  (binding [*parse-file-fn* parse-file
            *solve-fn* bl/solve-bb-best-first-top-down
            *create-output-fn* output-string]
    (apply solver-main args)))

(defn benchmark
  [& args]
  (binding [*parse-file-fn* parse-file
            *solve-fn* bl/solve-bb-best-first-top-down]
    (apply benchmark-main args)))

(defn ^{ :author "Andrea Richiardi" }
  assert-solution-capacity
  "Calculates the total capacity of a solution."
  [{:keys [item-count capacity items] :as input-map}
   {:keys [opt obj taken-items] :as output-map}]
  {:pre [(= (count items) (count taken-items))]}
  (let [output-capacity (reduce (fn [cum zipped] (if (second zipped)
                                                  (+ cum (nth (first zipped) 1 0))
                                                  cum))
                                0
                                (map vector items taken-items))] 
    (println "Assert " capacity " >= " output-capacity) 
    (assert (>= capacity output-capacity))))

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
