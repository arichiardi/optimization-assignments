(ns ^{:author "Andrea Richiardi"
      :doc "Common functions to all the assignments."}
  assignments.common
  (:require [clojure.string :as string :refer [trim blank? split join]]
            [clojure.java.io :as io :refer [reader]]
            [clojure.pprint :as pp :refer [pprint]]
            [clojure.tools.cli :refer [parse-opts summarize]]
            [assignments.core :refer [solver-main
                                      benchmark-main
                                      *solve-fn*
                                      *create-output-fn*
                                      *parse-file-fn*]]))

(defn- parse-lines
  "Recursive parsing function accepting lines in the format v<space>w and
  returning a vector of [v w] vectors"
  ([line-seq max-lines] (parse-lines line-seq max-lines 0 []))
  ([line-seq max-lines current-line result]
   (if (and (seq line-seq) (< current-line max-lines))
     (let [[vx wx] (split (first line-seq) #"[\s\t]")]
       (recur (rest line-seq) max-lines (inc current-line) (conj result (vector (Long/parseLong vx) (Long/parseLong wx)))))
     result)))

(defn parse-file
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

(defn output-string
  "Parses the solution map and build the solution outputstring.
  The solution map should have the following format:
  {:obj x
   :opt [t or f]
   :taken-items (t f t f f f t ...)}."
  [solution]
  (let [{:keys [obj opt taken-items]} solution]
    (str obj " " (bool->long opt) "\n" (join " " (map bool->long taken-items)))))

(def generate-input #(parse-file (second (split %1 #"[ ]"))))
