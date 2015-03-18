(ns ^{:author "Andrea Richiardi"
      :doc "My attempt to thn knapsack problem, in Clojure."}
  knapsack.solver
  (:require [clojure.string :as string :refer [trim blank? split join]]
            [clojure.java.io :as io :refer [reader]]
            [clojure.pprint :as pp :refer [pprint]]
            [clojure.tools.cli :refer [parse-opts summarize]]
            [assignments.core :refer [solver-main *solve-fn* *create-output-fn* *parse-file-fn*]])
  (:gen-class))

(set! *warn-on-reflection* true)

(def repl-args "-f src/knapsack/data/ks_82_0")

(def cli-options
  ;; An option with a required argument
  [["-f" "--file FILE" "The name of the file to process"
    :default ""
    :parse-fn #(trim (str %))
    :validate [#(not (blank? %)) "There must be a file to process"]]])

(declare parse-file)
(declare solve-greedy)
(declare solve-dynamic-programming)
(declare output-string)

(defn -main
  [& args]
  (binding [*parse-file-fn* parse-file
            *solve-fn* solve-greedy
            *create-output-fn* output-string]
    (apply solver-main args)))

(defn parse-lines
  "Recursive parsing function accepting lines in the format v<space>w and
  returning a vector of [v w] vectors"
  ([line-seq] (parse-lines line-seq []))
  ([line-seq result]
   (if (seq line-seq)
     (let [[vx wx] (split (first line-seq) #"[\s\t]")]
       (recur (rest line-seq) (conj result (vector (Long/parseLong vx) (Long/parseLong wx)))))
     result)))

(defn- parse-file
  "Returns the following input map:
  {:item-count n, :capacity K,
   :items [[v_0 w_0] [v_1 w_1] ... [v_n-1 w_n-1]]}"
  [file-name]
  (with-open [rdr (reader file-name)]
    (let [lines (line-seq rdr)
          first-line (split (first lines) #"[\s\t]")
          rest-of-lines (rest lines)]
      {:item-count (Long/parseLong (first first-line))
       :capacity (Long/parseLong (second first-line))
       :items (parse-lines rest-of-lines)})))

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
    (str obj " " opt "\n" (join " " (map bool->long taken-items)))))

;; A greedy performance disaster
(defn- solve-greedy-iter
  ([items capacity] (solve-greedy-iter items capacity 0 0 []))
  ([items capacity cum-value cum-weight cum-result]
   (if-let [item (first items)]
     (let [value (first item)
           weight (second item)]
       (if (<= (+ cum-weight weight) capacity)
         (recur (rest items) capacity (+ value cum-value) (+ weight cum-weight) (conj cum-result true))
         (recur (rest items) capacity cum-value cum-weight (conj cum-result false))))
     {:obj cum-value :taken-items cum-result})))

(defn- ^{ :author "Coursera Team" } solve-greedy
  [input]
  (let [{:keys [item-count capacity items] :as input-map} input]
    (assoc (solve-greedy-iter items capacity) :opt false)))


;; Dynamic to the rescue

(defn- ^{ :author "Andrea Richiardi" } solve-dp-iter
  "A solver using dynamic programming and iterative processing."
  ([items capacity] (solve-greedy-iter items capacity 0 0 []))
  ([items capacity cum-value cum-weight cum-result]
   ) )

(defn- ^{ :author "Andrea Richiardi" } solve-dynamic-programming
  [input]
  (let [{:keys [item-count capacity items] :as input-map} input]
    (assoc (solve-dp-iter items capacity) :opt false)))
