(ns examples.todo
  (:require-macros [cambo.component.macros :refer [defcomponent defcontainer profile]])
  (:require [cambo.component :as comp :refer [props get-fragment]]
            [cambo.core :as core]
            [cambo.http :refer [http-datasource]]
            [cambo.model :as model]
            [cljsjs.react]
            [cljsjs.react.dom]
            [cljs.pprint :refer [pprint]]))


(enable-console-print!)

(defn tag [tag]
  (fn [props & children]
    (apply js/React.createElement tag props children)))

(def div (tag "div"))
(def pre (tag "pre"))
(def h1 (tag "h1"))
(def h3 (tag "h3"))
(def ul (tag "ul"))
(def li (tag "li"))
(def input (tag "input"))
(def button (tag "button"))

(defcontainer TodoDetails
              :fragments {:todo [:todo/id
                                 :todo/text
                                 :todo/complete]}
              (handleCompleteChange [this ev]
                                    (let [complete (get-in (props this) [:todo :todo/complete])]
                                      (comp/set-model this :todo {:todo/complete (not complete)})))
              (handleDeleteClick [this ev]
                                 (when-let [on-delete (get (props this) :on-delete)]
                                   (let [id (get-in (props this) [:todo :todo/id])]
                                     (on-delete id))))
              (render [this]
                      (let [{:keys [todo]} (props this)
                            {:keys [todo/text todo/complete]} todo]
                        (li nil
                            (div nil
                                 (input #js {:type "checkbox"
                                             :checked complete
                                             :onChange #(.handleCompleteChange this %)})
                                 (div nil text)
                                 (button #js {:onClick #(.handleDeleteClick this %)} "delete"))))))

(def todo-details (comp/factory TodoDetails))

(defcontainer TodoList
              :initial-variables {:count 10}
              :fragments {:user (fn [{:keys [count]}]
                                  [{:user/todos [{(core/range 0 count) [:todo/id
                                                                        (get-fragment TodoDetails :todo)]}
                                                 :length]}])}
              (handleTodoDelete [this todo-id]
                                (let [{:keys [count]} (comp/variables this)]
                                  (comp/call-model this
                                                   [:current-user :user/todos :todo/delete]
                                                   {:todo/id todo-id}
                                                   {:this [{(core/range 0 count) [:todo/id
                                                                                  :todo/text
                                                                                  :todo/complete]}
                                                           :length]})))
              (render [this]
                      (let [{:keys [user]} (props this)
                            {:keys [user/todos]} user]
                        (div nil
                             (ul nil
                                 (vec (for [idx (core/range-keys (core/range 0 10))
                                            :let [{:keys [todo/id] :as todo} (get todos idx)]
                                            :when todo]
                                        (todo-details {:todo todo
                                                       :key id
                                                       :on-delete #(.handleTodoDelete this %)}))))))))

(def todo-list (comp/factory TodoList))

(defcomponent TodoEntry
              (handleClick [this ev]
                           (.preventDefault ev)
                           (when-let [on-entry (get (props this) :on-entry)]
                             (on-entry "Here is a todo!")))
              (render [this]
                      (div nil
                           (input #js {:type "text"})
                           (button #js {:onClick #(.handleClick this %)}
                                   "create"))))

(def todo-entry (comp/factory TodoEntry))

(defcontainer TodoApp
              :fragments {:user [:user/id
                                 :user/name
                                 (get-fragment TodoList :user)]}
              (handleEntry [this text]
                           (let [user-id (get-in (props this) [:user :user/id])]
                             (comp/call-model this
                                              ;; or :current-user if you prefer!
                                              [:user/by-id user-id :user/todos :todo/add]
                                              {:todo/text text
                                               :todo/complete false}
                                              {:refs [:todo/id
                                                      :todo/text
                                                      :todo/complete]})))
              (render [this]
                      (let [{:keys [user]} (props this)
                            {:keys [user/name]} user]
                        (div nil
                             (h1 nil "Todos")
                             (h3 nil name)
                             (todo-entry {:on-entry #(.handleEntry this %)})
                             (todo-list {:user user})))))

(def todo-app (comp/factory TodoApp))

(defcontainer Answer
              :fragments {:answer [:answer/type
                                   :answer/state
                                   :answer/value]}
              (render [this] nil))

(defcontainer Field
              :fragments {:question (fn [_]
                                      [:question/id
                                       :question/type
                                       {:question/answer [(get-fragment Answer :answer)]}
                                       {:question/questions [{(core/range 0 10) [(get-fragment Field :question 4)]}]}])}
              (render [this] nil))

(def model (model/model {:datasource (http-datasource "http://localhost:4000/cambo"
                                                      {"X-CSRF-TOKEN" "abc123"})}))

(js/ReactDOM.render
  (comp/renderer {:queries {:user [:current-user]}
                  :container TodoApp
                  :model model})
  (.getElementById js/document "app"))
