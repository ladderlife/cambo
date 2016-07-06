(ns examples.todo
  (:require-macros [com.athuey.cambo.component.macros :refer [defcomponent defcontainer]])
  (:require [com.athuey.cambo.component :as comp :refer [props get-fragment expand-fragment]]
            [com.athuey.cambo.core :as core]
            [com.athuey.cambo.model :as model]
            [com.athuey.cambo.graph :as graph]
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

(defcontainer QuoteDetails
              {:fragments (fn [_]
                            {:quote [:quote/term
                                     :quote/policy
                                     :quote/monthly-premium
                                     {:quote/carrier [:carrier/name
                                                      :carrier/description]}]
                             :user  [:user/name]})
               :component (component
                            (render [this]
                                    (div nil
                                         (h1 nil "QuoteDetails")
                                         (pre nil (with-out-str
                                                    (cljs.pprint/pprint (props this)))))))})

(def quote-details (comp/factory QuoteDetails))

(defcontainer UserQuote
              {:fragments (fn [_]
                            {:user [:user/name
                                    :user/age
                                    :user/gender
                                    (get-fragment QuoteDetails :user)
                                    {:user/quote [(get-fragment QuoteDetails :quote)]}]})
               :component (component
                            (render [this]
                                    (let [{:keys [user]} (props this)
                                          quote (get-in user [:user/quote])]
                                      (div nil
                                           (h1 nil "UserQuote")
                                           (pre nil (with-out-str
                                                      (cljs.pprint/pprint (props this))))
                                           (quote-details {:user user
                                                           :quote quote
                                                           :look-ma "no-computed"})))))})

(defonce ds (-> (graph/set {} [{:current-user (core/ref [:user/by-id 1])}
                               {:user/by-id {1 {:user/name   "Huey"
                                                :user/age    13
                                                :user/gender :gender/male
                                                :user/quote  (core/ref [:quote/by-id 13])}}}
                               {:quote/by-id {13 {:quote/term 10
                                                  :quote/policy 250000
                                                  :quote/monthly-premium 13.52
                                                  :quote/carrier (core/ref [:carrier/by-code :lddr])}}}
                               {:carrier/by-code {:lddr {:carrier/name "Ladder"
                                                         :carrier/description "11 badasses"}}}])
                :graph
                (graph/as-datasource)))

(defonce model (model/model {:datasource ds}))

(js/ReactDOM.render
  (comp/renderer {:queries {:user [:current-user]}
                  :container UserQuote
                  :model model})
  (.getElementById js/document "app"))
