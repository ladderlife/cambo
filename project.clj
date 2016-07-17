(defproject com.ladderlife/cambo "0.1.0-SNAPSHOT"
  :description "A Clojure take on Netflix's Falcor"
  :url "https://github.com/eyston/cambo"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.8.0" :scope "provided"]
                 [org.clojure/clojurescript "1.8.51" :scope "provided"]
                 [cljsjs/react "15.0.1-1"]
                 [cljsjs/react-dom "15.0.1-1"]
                 [com.cognitect/transit-clj "0.8.285"]
                 [com.cognitect/transit-cljs "0.8.239"]]
  :plugins [[lein-figwheel "0.5.4-3"]
            [lein-cljsbuild "1.1.3"]]
  :jar-exclusions [#"examples/" #"public/"]
  :profiles {:dev {:dependencies
                   [[org.clojure/clojure "1.8.0"]
                    [org.clojure/clojurescript "1.8.51"]]}
             :examples {:dependencies
                        [[org.clojure/clojure "1.8.0"]
                         [org.clojure/clojurescript "1.8.51"]
                         [clj-http "2.2.0"]
                         [cheshire "5.6.2"]
                         [environ "1.0.3"]
                         [ring "1.5.0"]
                         [criterium "0.4.4"]]}}
  :cljsbuild {:builds [{:id "dev"
                        :source-paths ["src"]
                        :figwheel true
                        :compiler {:main "examples.todo"
                                   :asset-path "cljs/main-out"
                                   :output-to  "resources/public/cljs/main.js"
                                   :output-dir "resources/public/cljs/main-out"}}
                       {:id "prod"
                        :source-paths ["src"]
                        :figwheel true
                        :compiler {:main "examples.todo"
                                   :asset-path "cljs/out"
                                   :output-to  "resources/public/cljs/prod.js"
                                   :output-dir "resources/public/cljs/prod-out"
                                   :optimizations :advanced}}]})
