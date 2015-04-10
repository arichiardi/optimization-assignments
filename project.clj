(defproject assignments "0.1.0-SNAPSHOT"
  :description "Coursera - Discrete Optimization assignments"
  :url "https://class.coursera.org/optimization-003"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [org.clojure/tools.cli "0.3.1"]
                 [org.clojure/data.int-map "0.1.0"]
                 [org.clojure/tools.trace "0.7.8"]
                 [com.taoensso/timbre "3.4.0"]
                 [rhizome "0.2.4"]
                 [criterium "0.4.3"]]
  :plugins [[cider/cider-nrepl "0.8.2"]
            [refactor-nrepl "0.3.0-SNAPSHOT"]]
  :main knapsack.solver
  :jvm-opts ^:replace ["-Xmx4g" "-XX:+UseConcMarkSweepGC" "-XX:-OmitStackTraceInFastThrow"]
  :aot :all)
