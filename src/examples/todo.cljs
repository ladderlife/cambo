(ns examples.todo
  (:require-macros [cambo.component.macros :refer [defcomponent defcontainer]])
  (:require [cambo.component :as comp :refer [props get-fragment expand-fragment]]
            [cambo.core :as core]
            [cambo.model :as model]
            [cambo.graph :as graph]
            [cambo.router :as router :refer [RANGES INTEGERS KEYS]]
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
              {:fragments {:todo [:todo/id
                                  :todo/text
                                  :todo/complete]}
               :component (component
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
                                               (button #js {:onClick #(.handleDeleteClick this %)} "delete"))))))})

(def todo-details (comp/factory TodoDetails))

(defcontainer TodoList
              {:initial-variables {:count 10}
               :fragments {:user (fn [{:keys [count]}]
                                   [{:user/todos [{(core/range 0 count) [:todo/id
                                                                         (get-fragment TodoDetails :todo)]}
                                                  :length]}])}
               :component (component
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
                                                                     :on-delete #(.handleTodoDelete this %)}))))))))})

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
              {:fragments {:user [:user/id
                                  :user/name
                                  (get-fragment TodoList :user)]}
               :component (component
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
                                           (todo-list {:user user})))))})

(def todo-app (comp/factory TodoApp))

;; pretend these are database functions
(def user-db (atom {13 {:user/id 13
                        :user/name "Huey"
                        :user/age 35
                        :user/gender :gender/male
                        :user/todos [1 2 3]}}))

(defn get-users
  [users ids]
  (select-keys @users ids))

(defn get-user
  [users id]
  (get @users id))

(defn add-user-todo
  [users id todo-id]
  (swap! users update-in [id :user/todos] (fnil conj []) todo-id)
  (dec (count (get-in @users [id :user/todos]))))

(defn remove-user-todo
  [users id todo-id]
  (let [idx (.indexOf (get-in @users [id :user/todos]) todo-id)]
    (swap! users update-in [id :user/todos] (fn [todos]
                                              (into [] (remove #{todo-id} todos))))
    idx))

(def todo-db (atom {1 {:todo/id 1
                       :todo/text "Buy a unicorn"
                       :todo/complete false}
                    2 {:todo/id 2
                       :todo/text "Learn an clojurescript"
                       :todo/complete false}
                    3 {:todo/id 3
                       :todo/text "pokemon"
                       :todo/complete true}}))

(defn get-todos
  [todos ids]
  (select-keys @todos ids))

(defn get-todo
  [todos id]
  (get @todos id))

(defn todo-set-complete
  [todos id complete]
  (swap! todos assoc-in [id :todo/complete] complete))

(def todo-ids (atom 3))

(defn create-todo
  [todos args]
  (let [id (swap! todo-ids inc)]
    (swap! todos assoc id (merge {:todo/id id}
                                 args))
    id))

(defn delete-todo
  [todos id]
  (swap! todos dissoc id))

(def routes
  [{:route [:current-user]
    :get (fn [_ {:keys [session/user-id]}]
           [(core/path-value [:current-user]
                             (core/ref [:user/by-id user-id]))])}

   {:route [:user/by-id INTEGERS [:user/id :user/name :user/age :user/gender]]
    :get (fn [[_ ids keys] {:keys [db/users]}]
           (for [[id user] (get-users users ids)]
             {:user/by-id {id (select-keys user keys)}}))}

   {:route [:user/by-id INTEGERS :user/todos RANGES]
    :get (fn [[_ ids _ ranges] {:keys [db/users]}]
           (for [[id {:keys [user/todos]}] (get-users users ids)
                 idx (router/indices ranges)
                 :let [todo-id (get todos idx)]
                 :when todo-id]
             (core/path-value [:user/by-id id :user/todos idx]
                              (core/ref [:todo/by-id todo-id]))))}

   {:route [:user/by-id INTEGERS :user/todos :length]
    :get (fn [[_ ids] {:keys [db/users]}]
           (for [[id {:keys [user/todos]}] (get-users users ids)]
             (core/path-value [:user/by-id id :user/todos :length]
                              (count todos))))}

   {:route [:todo/by-id INTEGERS [:todo/id :todo/text :todo/complete]]
    :get (fn [[_ ids keys] {:keys [db/todos]}]
           (for [[id todo] (get-todos todos ids)]
             {:todo/by-id {id (select-keys todo keys)}}))}

   {:route [:todo/by-id INTEGERS :todo/complete]
    :set (fn [pathmap {:keys [db/todos]}]
           (doseq [[id complete] (:todo/by-id pathmap)]
             (todo-set-complete todos id complete))
           [pathmap])}

   {:route [:user/by-id INTEGERS :user/todos :todo/add]
    :call (fn [[_ [id]] args {:keys [db/todos db/users]}]
            (let [todo-id (create-todo todos args)
                  idx (add-user-todo users id todo-id)]
              [(core/path-value [:user/by-id id :user/todos idx]
                                (core/ref [:todo/by-id todo-id]))
               ;; this should be refPaths
               {:todo/by-id {todo-id (merge {:todo/id todo-id}
                                            args)}}]))}

   {:route [:user/by-id INTEGERS :user/todos :todo/delete]
    :call (fn [[_ [id]] args {:keys [db/todos db/users]}]
            (let [todo-id (:todo/id args)
                  idx (remove-user-todo users id todo-id)]
              (delete-todo todos todo-id)
              (into [(core/invalidate [:user/by-id id :user/todos idx])
                     (core/invalidate [:user/by-id id :user/todos])
                     (core/invalidate [:todos/by-id todo-id :user/todos idx])]
                     ;; this should be thisPaths
                     (for [[idx todo-id] (map-indexed vector (:user/todos (get-user users id)))]
                       (core/path-value [:user/by-id id :user/todos idx]
                                        (core/ref [:todo/by-id todo-id]))))))}])

(def router (router/router routes))

(def model (model/model {:datasource (router/as-datasource router {:session/user-id 13
                                                                   :db/users user-db
                                                                   :db/todos todo-db})}))

(js/ReactDOM.render
  (comp/renderer {:queries {:user [:current-user]}
                  :container TodoApp
                  :model model})
  (.getElementById js/document "app"))
