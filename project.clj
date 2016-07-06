(defproject com.athuey/cambo "0.1.0-SNAPSHOT"
  :description "A Clojure take on Netflix's Falcor"
  :url "https://github.com/eyston/cambo"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [org.clojure/clojurescript "1.8.51"]
                 [cljsjs/react "15.0.1-1"]
                 [cljsjs/react-dom "15.0.1-1"]]
  :plugins [[lein-figwheel "0.5.4-3"]]
  :profiles {:dev {:dependencies
                   [[org.clojure/clojure "1.8.0"]
                    [org.clojure/clojurescript "1.8.51"]
                    [cljsjs/react "15.0.1-1"]
                    [cljsjs/react-dom "15.0.1-1"]]}
             :examples {:dependencies
                        [[org.clojure/clojure "1.7.0"]
                         [org.clojure/clojurescript "1.8.51"]
                         [clj-http "2.2.0"]
                         [cheshire "5.6.2"]
                         [environ "1.0.3"]]}}
  :cljsbuild {:builds [{:id "dev"
                        :source-paths ["src"]
                        :figwheel true
                        :compiler {:main "examples.todo"
                                   :asset-path "cljs/out"
                                   :output-to  "resources/public/cljs/main.js"
                                   :output-dir "resources/public/cljs/out"}}]})
