(ns ^{:author "Andrea Richiardi"
      :doc "My attempt to the knapsack problem, in Clojure."}
  knapsack.solver
  (:require :reload-all [clojure.string :as string :refer [trim blank? split join]]
            [clojure.java.io :as io :refer [reader]]
            [clojure.pprint :as pp :refer [pprint]]
            [clojure.tools.cli :refer [parse-opts summarize]]
            [assignments.core :refer [solver-main
                                      benchmark-main
                                      *solve-fn*
                                      *create-output-fn*
                                      *parse-file-fn*]]
            [knapsack.dp :as dp])
  (:gen-class))

(set! *warn-on-reflection* true)

(declare parse-file)
(declare output-string)
(declare solve-greedy)

(def repl-args "-f src/knapsack/data/ks_lecture_dp_2")
(def generate-input #(parse-file (second (split %1 #"[ ]"))))

(defn -main
  "Solves the problem at hand and prints the result on stdout."
  [& args]
  (binding [*parse-file-fn* parse-file
            *solve-fn* dp/solve-dp-memory-conscious-iterative
            *create-output-fn* output-string]
    (apply solver-main args)))

(defn benchmark
  [& args]
  (binding [*parse-file-fn* parse-file
            *solve-fn* dp/solve-dp-memory-conscious-iterative]
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
   :taken-items (t f t f f f t ...)}."
  [solution]
  (let [{:keys [obj opt taken-items]} solution]
    (str obj " " (bool->long opt) "\n" (join " " (map bool->long taken-items)))))

(defn- ^{ :author "Andrea Richiardi" }
  solution-capacity
  "Calculates the total capacity of a solution."
  [{:keys [item-count capacity items] :as input-map}
   {:keys [opt obj taken-items] :as output-map}]
  {:pre [(= (count items) (count taken-items))]}
  (reduce (fn [cum zipped] (if (second zipped)
                            (+ cum (nth (first zipped) 1 0))
                            cum))
          0
          (map vector items taken-items)))

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
