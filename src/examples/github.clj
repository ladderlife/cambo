(ns examples.github
  (:require [cambo.core :as core :refer [path-value]]
            [cambo.router :as router :refer [KEYS INTEGERS RANGES]]
            [cambo.model :as model]
            [clj-http.client :as http]
            [clojure.edn :as edn]
            [environ.core :refer [env]]))

(def base-url "https://api.github.com")

(def base-options {:accept :json
                   :as :json
                   :query-params {"client_id" (env :github-client-id)
                                  "client_secret" (env :github-client-secret)}})

(defn api-get [resource]
  (:body (http/get (str base-url resource) base-options)))

(defn api-count [resource]
  (let [result (http/get (str base-url resource "?per_page=1") base-options)]
    (some->> (get-in result [:links :last :href])
             (re-find #"&page=(\d+)")
             second
             edn/read-string)))

(def routes
  [{:route [:org/by-id INTEGERS [:org/description
                                 :org/email
                                 :org/name
                                 :org/login
                                 :org/id
                                 :org/url]]
    :get (fn [[_ ids keys] _]
           (for [id ids
                 :let [org (api-get (str "/organizations/" id))]
                 :when org
                 key keys
                 :let [github-key (keyword (name key))]]
             (path-value [:org/by-id id key]
                         (get org github-key))))}

   {:route [:org/by-id INTEGERS :org/repos RANGES]
    :get (fn [[_ ids _ ranges] _]
           (for [id ids
                 :let [repos (into [] (api-get (str "/organizations/" id "/repos")))]
                 idx (router/indices ranges)
                 :let [repo (get repos idx)]
                 :when repo]
             (path-value [:org/by-id id :org/repos idx]
                         (core/ref [:repo/by-id (:id repo)]))))}

   {:route [:org/by-id INTEGERS :org/repos :length]
    :get (fn [[_ ids _ _] _]
           (for [id ids
                 :let [repo-count (api-count (str "/organizations/" id "/repos"))]
                 :when repo-count]
             (path-value [:org/by-id id :org/repos :length]
                         repo-count)))}

   {:route [:repo/by-id INTEGERS [:repo/description
                                  :repo/homepage
                                  :repo/name
                                  :repo/full-name
                                  :repo/stargazers-count
                                  :repo/size
                                  :repo/language
                                  :repo/id
                                  :repo/url
                                  :repo/forks
                                  :repo/owner]]
    :get (fn [[_ ids keys] _]
           (for [id ids
                 :let [repo (api-get (str "/repositories/" id))]
                 :when repo
                 key keys
                 :let [github-key (keyword (name key))]]
             (path-value [:repo/by-id id key]
                         (case key
                           :repo/full-name (get repo :full_name)
                           :repo/stargazers-count (get repo :stargazers_count)
                           :repo/owner (let [owner (get repo :owner)]
                                         (case (:type owner)
                                           "Organization" (core/ref [:org/by-id (:id owner)])))
                           (get repo github-key)))))}])

(comment

  (let [router (router/router routes)]
    (router/get router [[:org/by-id 913567 [:org/name :org/description]]]))

  (let [router (router/router routes)]
    (router/get router [[:org/by-id [913567] :org/repos :length]
                        [:org/by-id [913567] :org/repos (core/range 0 1) [:repo/description
                                                                          :repo/name
                                                                          :repo/full-name
                                                                          :repo/forks
                                                                          :repo/stargazers-count]]
                        [:org/by-id [913567] :org/repos (core/range 0 1) :repo/owner [:org/name
                                                                                      :org/description]]]))

  (def query [{:org/by-id [{913567 [{:org/repos [:length
                                                 {(core/range 0 1) [:repo/description
                                                                    :repo/name
                                                                    :repo/full-name
                                                                    :repo/forks
                                                                    :repo/stargazers-count
                                                                    {:repo/owner [:org/name
                                                                                  :org/description]}]}]}]}]}])

  (defn pull [query]
    (let [leafs (into [] (remove map? query))
          paths (for [join (filter map? query)
                      :let [[key query] (first join)]
                      paths (pull query)]
                  (into [key] paths))]
      (cond-> (into [] paths)
              (seq leafs) (conj [leafs]))))

  (pull [:repo/description
         :repo/name
         {:repo/owner [:org/name
                       :org/id]}
         {:repo/foo [:org/name
                     :org/id
                     {:org/repos [:length]}]}])

  (pull [{:org/by-id [{913567 [{:org/repos [:length
                                            {(core/range 0 1) [:repo/description
                                                               :repo/name
                                                               :repo/full-name
                                                               :repo/forks
                                                               :repo/stargazers-count
                                                               {:repo/owner [:org/name
                                                                             :org/description]}]}]}]}]}])

  (let [router (router/router routes)]
    (router/get router (pull [{:org/by-id [{913567 [{:org/repos [:length
                                                                 {(core/range 0 1) [:repo/description
                                                                                    :repo/name
                                                                                    :repo/full-name
                                                                                    :repo/forks
                                                                                    :repo/stargazers-count
                                                                                    {:repo/owner [:org/name
                                                                                                  :org/description]}]}]}]}]}])))

  (let [router (router/router routes)
        model (model/model {:datasource (router/as-datasource router)})
        result (promise)]
    (model/get model [[:org/by-id 913567 [:org/name :org/description]]
                      [:org/by-id 913567 :org/repos (core/range 0 5) [:repo/name :repo/description]]]
               (partial deliver result))
    (deref result 500 :timeout))

  (let [m (model/model {:datasource (router/as-datasource (router/router routes))})
        result (promise)]
    (model/get m (pull [{:org/by-id [{913567 [:org/name
                                              :org/description
                                              {:org/repos [:length
                                                           {(core/range 0 2) [:repo/description
                                                                              :repo/name
                                                                              :repo/full-name
                                                                              :repo/forks
                                                                              :repo/stargazers-count
                                                                              {:repo/owner [:org/name
                                                                                            :org/description]}]}]}]}]}])
               (partial deliver result))
    (deref result 500 :timeout))

  )
