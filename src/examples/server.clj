(ns examples.server
  (:require [ring.adapter.jetty :refer [run-jetty]]
            [cambo.core :as core]
            [cambo.ring :refer [wrap-cambo]]
            [cambo.router :as router :refer [RANGES INTEGERS KEYS]]))

;; DATABASE

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

;; ROUTES

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
           (doseq [[id {:keys [todo/complete]}] (:todo/by-id pathmap)]
             (todo-set-complete todos id complete))
           [pathmap])}

   {:route [:user/by-id INTEGERS :user/todos :todo/add]
    :call (fn [[_ [id]] args {:keys [db/todos db/users]}]
            (let [todo-id (create-todo todos args)
                  idx (add-user-todo users id todo-id)]
              [(core/path-value [:user/by-id id :user/todos idx]
                                (core/ref [:todo/by-id todo-id]))]))}

   {:route [:user/by-id INTEGERS :user/todos :todo/delete]
    :call (fn [[_ [id]] args {:keys [db/todos db/users]}]
            (let [todo-id (:todo/id args)
                  idx (remove-user-todo users id todo-id)]
              (delete-todo todos todo-id)
              [(router/invalidate [:user/by-id id :user/todos])
               (router/invalidate [:todos/by-id todo-id :user/todos idx])]))}])

;; ROUTER

(def router (router/router routes))

;; HTTP

(defn cambo-handler
  [{:keys [cambo context]}]
  (router/handle router cambo context))

(defn wrap-cors
  [handler]
  (fn [{:keys [request-method] :as request}]
    (let [response (if (= :options request-method)
                     {:status 200}
                     (handler request))]
      (update response :headers merge {"Access-Control-Allow-Origin" "*"
                                       "Access-Control-Allow-Headers" "Content-Type"}))))

(def cambo-handler' (-> cambo-handler wrap-cambo wrap-cors))

(defn handler [{:keys [uri] :as request}]
  (let [request (assoc request :context {:session/user-id 13
                                         :db/users user-db
                                         :db/todos todo-db})]
    (case uri
      "/cambo" (cambo-handler' request)
      {:status 200
       :headers {"Content-Type" "text/html"}
       :body uri})))

(defonce server nil)

(defn start-server
  []
  (when server
    (.stop server))
  (alter-var-root
    #'server
    (fn [_]
      (run-jetty handler
                 {:host "0.0.0.0" :port 4000 :join? false}))))

(start-server)
