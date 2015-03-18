(ns assignments.core
  (:require [clojure.string :as string :refer [trim blank?]]
            [clojure.java.io :as io :refer [reader]]
            [clojure.pprint :as pp :refer [pprint]]
            [clojure.tools.cli :refer [parse-opts summarize]]))

(declare cli-options)
(declare error-msg)
(declare exit)

(def ^{:dynamic true
       :doc "Parses the input file, accepts a string with the file name and returns an input map, in
the format specified by the problem and so that *solve-fn* can read." }
  *parse-file-fn* nil)

(def ^{:dynamic true
       :doc "Solving function symbol, rebindable, which returns the solution map"}
  *solve-fn* nil)

(def ^{:dynamic true
       :doc "Tranforms the solution map in a string, which will be printed to stdout by
 solver-main." }
  *create-output-fn* nil)

(defn solver-main
  "Main function to call in order to solve a problem, re-binding
  *parse-file-fn*, *solve-fn* *create-output-fn* to custom code"
  [& args]
  (let [{:keys [options arguments errors summary]} (parse-opts args cli-options)]
    (cond
     (not= (count arguments) 0) (exit 1 (summarize cli-options))
     errors (exit 1 (error-msg errors))
     :else (println (*create-output-fn* (*solve-fn* (*parse-file-fn* (options :file))))))))

(def ^{:private true} cli-options
  ;; An option with a required argument
  [["-f" "--file FILE" "The name of the file to process"
    :default ""
    :parse-fn #(trim (str %))
    :validate [#(not (blank? %)) "There must be a file to process"]]])

(defn- error-msg [errors]
  (str "The following errors occurred while parsing your command:\n\n"
       (string/join \newline errors)))

(defn- exit [status msg]
  (println msg)
  (System/exit status))
