(ns com.athuey.cambo.model
  (:refer-clojure :exclude [get set])
  (:require [com.athuey.cambo.core :as core]
            [com.athuey.cambo.graph :as graph]))

(defrecord Model
  [cache datasource])

(defn model [{:keys [cache datasource]}]
  (Model. (atom (->> [(or cache {})]
                     (graph/set {})
                     :graph))
          datasource))

(defn get-cache
  ([{:keys [cache]}]
   @cache)
  ([{:keys [cache]} pathsets]
   (let [c @cache]
     (:graph (graph/get c pathsets {:normalize false})))))

(defn set-cache
  [{:keys [cache]} pathmaps]
  (with-local-vars [ps nil]
    (let [c (swap! cache (fn [cache]
                           (let [{:keys [graph paths]} (graph/set cache pathmaps)]
                             (var-set ps paths)
                             graph)))]
      (:graph (graph/get c @ps {:normalize false})))))

;; TODO: look at how falcor handles boxing / normalization for intermediate results / final results

(defn get
  [{:keys [cache datasource] :as m} pathsets]
  (let [{:keys [graph missing]} (graph/get @cache pathsets {:normalize false})]
    (if (seq missing)
      (let [result (core/get datasource missing)
            pathmaps [(:graph result)]]
        (set-cache m pathmaps)
        (:graph (graph/get @cache pathsets {:normalize false})))
      graph)))

(defn set
  [{:keys [datasource] :as m} pathmaps]
  (set-cache m pathmaps)
  (let [{:keys [paths] :as r} (core/set datasource pathmaps)]
    ;; TODO: set cache again with result?
    (get-cache m paths)))

;; TODO: rest of falcor model interface as necessary
