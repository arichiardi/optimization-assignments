(defproject assignments "0.1.0-SNAPSHOT"
  :description "Coursera - Discrete Optimization assignments"
  :url "https://class.coursera.org/optimization-003"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [org.clojure/tools.cli "0.3.1"]]
  :plugins [[cider/cider-nrepl "0.8.2"]]
  :main knapsack.solver
  :aot :all)
