(defproject assignments "0.1.0-SNAPSHOT"
  :description "Coursera - Discrete Optimization assignments"
  :url "https://class.coursera.org/optimization-003"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.7.0-beta1"]
                 [org.clojure/tools.cli "0.3.1"]
                 [org.clojure/data.int-map "0.1.0"]
                 [com.taoensso/timbre "3.4.0"]
                 [rhizome "0.2.6-SNAPSHOT"]
                 [criterium "0.4.3"]
                 [loco "0.2.1"]]
  :main coloring.solver
  :jvm-opts ^:replace ["-Xmx4g" "-XX:+UseConcMarkSweepGC" "-XX:-OmitStackTraceInFastThrow"]
  :aot [coloring.solver])
