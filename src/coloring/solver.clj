(ns ^{:author "Andrea Richiardi"
      :doc "My attempt to the coloring problem, in Clojure."}
  coloring.solver
  (:require [clojure.string :as string :refer [trim blank? split join]]
            [clojure.java.io :as io :refer [reader]]
            [clojure.pprint :as pp :refer [pprint]]
            [clojure.tools.cli :refer [parse-opts summarize]]
            [assignments.common :refer [parse-file
                                        output-string
                                        generate-input]]
            [assignments.core :refer [solver-main
                                      benchmark-main
                                      *solve-fn*
                                      *create-output-fn*
                                      *parse-file-fn*]])
  (:gen-class))

(set! *warn-on-reflection* true)

(defn -main
  "Solves the problem at hand and prints the result on stdout."
  [& args]
  (binding [*parse-file-fn* parse-file
            *solve-fn* nil
            *create-output-fn* output-string]
    (apply solver-main args)))

(defn benchmark
  [& args]
  (binding [*parse-file-fn* parse-file
            *solve-fn* nil]
    (apply benchmark-main args)))
