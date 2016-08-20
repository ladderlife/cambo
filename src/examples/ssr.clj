(ns examples.ssr
  (:require [cambo.component :as comp :refer [defcomponent defcontainer]]
            [cambo.core :as cam]
            [cambo.model :as model]))

(defcomponent NotFound
              (render [this]
                      (js/console.log "Hello world")))

(defcontainer Todo
              :fragments {:todo [:todo/id
                                 :todo/text
                                 :todo/complete?]}
              (render [this]
                      (js/console.log "Hello from Todos!")))

(defcontainer TodoList
              :fragments {:todos [{(cam/range 0 10) [:todo/id
                                                     (comp/get-fragment Todo :todo)]}]}
              (render [this]
                      (js/console.log "Hello from Todos!")))

(macroexpand '(defcontainer TodoList
                            :fragments {:todos [{(cam/range 0 10) [:todo/id
                                                                   :todo/text
                                                                   :todo/complete?]}]}
                            (render [this]
                                    #js {}
                                    (js/console.log "Hello from Todos!"))))

(let [fragment (comp/get-fragment TodoList :todos)]
  (comp/full-fragment fragment))

(comp/build-query {:todos [:todos/all]} TodoList)

(comment

  (let [model (model/model {:cache {:foo {:bar "baz"}}})]
    @(model/pull model [{:foo [:bar]}]))

  (let [model (model/model {:cache {:foo {:bar "baz"}}})]
    (model/pull model [{:foo [:bar]}] (fn [value]
                                        (println "value!" value))))

  )
