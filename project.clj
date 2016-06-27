(defproject com.athuey/cambo "0.1.0-SNAPSHOT"
  :description "A Clojure take on Netflix's Falcor"
  :url "https://github.com/eyston/cambo"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies []
  :profiles {:dev {:dependencies
                   [[org.clojure/clojure "1.7.0"]]}
             :examples {:dependencies
                        [[org.clojure/clojure "1.7.0"]
                         [clj-http "2.2.0"]
                         [cheshire "5.6.2"]
                         [environ "1.0.3"]]
                        :source-paths ["examples"]}})
