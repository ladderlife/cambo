(ns com.athuey.cambo.model
  (:refer-clojure :exclude [get set])
  (:require [com.athuey.cambo.core :as core]
            [com.athuey.cambo.graph :as graph]))

(defrecord Model
  [cache datasource])

(defn model [{:keys [cache] :as opts}]
  (map->Model (assoc opts :cache (atom (->> [(or cache {})]
                                            (graph/set {})
                                            :graph)))))

(defn with-path-info [model]
  (assoc model :path-info true))

(defn get-options
  ([model]
    (get-options model nil))
  ([model opts]
    (merge (select-keys model [:path-info :boxed])
           {:normalize false}
           opts)))

(defn get-cache
  ([{:keys [cache]}]
   @cache)
  ([{:keys [cache] :as m} pathsets]
   (let [c @cache]
     (:graph (graph/get c pathsets (get-options m))))))

(defn set-cache
  [{:keys [cache] :as m} pathmaps]
  #?(:cljs (let [ps (atom nil)
                 c (swap! cache (fn [cache]
                                  (let [{:keys [graph paths]} (graph/set cache pathmaps)]
                                    (reset! ps paths)
                                    graph)))]
             (:graph (graph/get c @ps {:normalize false})))
     :clj (with-local-vars [ps nil]
            (let [c (swap! cache (fn [cache]
                                   (let [{:keys [graph paths]} (graph/set cache pathmaps)]
                                     (var-set ps paths)
                                     graph)))]
              (:graph (graph/get c @ps {:normalize false}))))))

;; TODO: look at how falcor handles boxing / normalization for intermediate results / final results

(defn get
  ;; TODO: single call cb, or subscription w/ readystate?
  ;; TODO: loop?
  [{:keys [cache datasource] :as m} pathsets cb]
  (let [{:keys [graph missing]} (graph/get @cache pathsets (get-options m))]
    (if (seq missing)
      (core/get datasource missing (fn [{:keys [graph]}]
                                     (set-cache m [graph])
                                     (cb (:graph (graph/get @cache pathsets (get-options m))))))
      (cb graph)))
  nil)

(defn set
  [{:keys [datasource] :as m} pathmaps cb]
  (set-cache m pathmaps)
  ;; TODO: set cache again with result?
  (core/set datasource pathmaps (fn [{:keys [paths]}]
                                  (cb (get-cache m paths)))))

;; TODO: rest of falcor model interface as necessary
