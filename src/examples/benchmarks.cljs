(ns examples.benchmarks
  (:require-macros [cambo.component :refer [defcomponent defcontainer]])
  (:require [examples.queries :as queries]
            [cognitect.transit]
            [cambo.core :as core]
            [cambo.router]
            [cambo.graph :as graph]
            [cambo.model :as model]
            [cambo.component]
            [cljs.pprint :refer [pprint]]
            [cljsjs.benchmark]))

(enable-console-print!)

(def suite (js/Benchmark.Suite))

(deftype EmptyDataSource [])

(extend-type EmptyDataSource
  core/IDataSource
  (get [this pathsets cb])
  (set [this pathmaps cb])
  (call [this path args queries cb]))

(def ds (EmptyDataSource.))

(def opts {:normalize false
           :path-info false
           :boxed false})

(println "options" opts)

(doseq [[name queries pulls] [["application" queries/application-queries queries/application-pulls]
                              ["questionnaire" queries/questionnaire-queries queries/questionnaire-pulls]
                              ["dashboard" queries/dashboard-queries queries/dashboard-pulls]]
        :let [pathsets (reduce into queries)
              model (model/model {:cache queries/full-uwb
                                  :datasource ds})]]

  (doseq [idx (range 0 (count queries))
          :let [pathsets (nth queries idx)]]
    (.add suite (str name ":get:" idx) (fn []
                                         (graph/get queries/full-uwb pathsets opts))))

  (doseq [idx (range 0 (count pulls))
          :let [query (nth pulls idx)]]
    (.add suite (str name ":pull:" idx) (fn []
                                          (graph/pull queries/full-uwb query opts))))

  (.add suite (str name ":get:all") (fn []
                                      (graph/get queries/full-uwb pathsets opts)))

  (.add suite (str name ":missing") (fn []
                                      (doall (graph/missing queries/full-uwb pathsets))))

  (.add suite (str name ":missing-transient") (fn []
                                                (doall (graph/missing-transient queries/full-uwb pathsets))))

  (.add suite (str name ":prime") (fn []
                                    (model/prime model pathsets (fn [_])))))

(.add suite "set" (fn []
                    (graph/set {} [queries/full-uwb])))

(.on suite "complete" (fn []
                        (this-as this
                          (doseq [idx (range 0 (.-length this))
                                  :let [bench (aget this idx)]]
                            (println (.-name bench)
                                     "mean:" (* 1000 (.. bench -stats -mean)) "ms")))))

(defn ^:export run []
  (.run suite)
  nil)

(let [pathsets (reduce into queries/questionnaire-queries)
      query (reduce into [] queries/questionnaire-pulls)]
  (println (= (:graph (graph/get queries/full-uwb pathsets opts))
              (:graph (graph/pull queries/full-uwb query opts))))
  (time (graph/get queries/full-uwb pathsets opts))
  (time (graph/pull queries/full-uwb query opts)))


;(let [pathsets (reduce into queries/application-queries)
;      query (reduce into [] queries/application-pulls)]
;  (println (= (:graph (graph/get queries/full-uwb pathsets opts))
;              (:graph (graph/pull queries/full-uwb query opts))))
;  (time (graph/get queries/full-uwb pathsets opts))
;  (time (graph/pull queries/full-uwb query opts)))
;
;(let [pathsets (reduce into queries/dashboard-queries)
;      query (reduce into [] queries/dashboard-pulls)]
;  (println (= (:graph (graph/get queries/full-uwb pathsets opts))
;              (:graph (graph/pull queries/full-uwb query opts))))
;  (time (graph/get queries/full-uwb pathsets opts))
;  (time (graph/pull queries/full-uwb query opts)))

;(let [pathsets (reduce into queries/questionnaire-queries)]
;  (time (:missing (graph/get queries/full-uwb pathsets opts)))
;  (time (graph/missing queries/full-uwb pathsets))
;  (time (graph/missing-transient queries/full-uwb pathsets))
;
;  (time (:missing (graph/get {} pathsets opts)))
;  (time (graph/missing {} pathsets))
;  (time (graph/missing-transient {} pathsets)))
