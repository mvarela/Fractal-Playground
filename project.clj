(defproject fractals "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [hiccup "1.0.5"]
                 [com.evocomputing/colors "1.0.3"]]
  :main ^:skip-aot fractals.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
