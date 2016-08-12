(ns examples.ssr
  (:require [cambo.component :as comp :refer [defcomponent defcontainer]]
            [cambo.core :as cam]))

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
  (comp/pathsets fragment))

(comp/query-pathsets {:todos [:todos/all]} TodoList)
