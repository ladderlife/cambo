(ns com.athuey.cambo.graph
  (:refer-clojure :exclude [get set atom ref keys])
  (:require [com.athuey.cambo.core :refer [boxed? atom atom? ref? keys] :as core]))

(defn get
  ([graph pathsets]
   (get graph pathsets {:normalize false
                        :boxed false}))
  ([graph pathsets {:keys [normalize boxed]}]
   (letfn [(get-pathset [node path opt result [ks & pathset :as ps]]
             ;; TODO: might be able to flatten this kinda like set
             (cond
               (nil? node)
               (update result :missing conj (into opt ps))

               (boxed? node)
               result

               (seq pathset)
               (reduce (fn [result k]
                         (let [node' (clojure.core/get node k)]
                           (if (ref? node')
                             (let [result (if normalize
                                            (assoc-in result (into [:graph] (conj opt k)) node')
                                            result)
                                   path' (:path node')]
                               (get-pathset (get-in graph path')
                                            (conj path k)
                                            path'
                                            result
                                            pathset))
                             (get-pathset node'
                                          (conj path k)
                                          (conj opt k)
                                          result
                                          pathset))))
                       result
                       (keys ks))

               :else
               (reduce (fn [result k]
                         (let [value (clojure.core/get node k)]
                           ;; TODO: do we want to require boxed graph?
                           (if (atom? value)
                             (assoc-in result
                                       (into [:graph] (if normalize
                                                        (conj opt k)
                                                        (conj path k)))
                                       (if boxed
                                         value
                                         (:value value)))
                             (update result :missing conj (conj opt k)))))
                       result
                       (keys ks))))]

     (reduce (partial get-pathset graph [] [])
             {:graph {}
              :missing []}
             pathsets))))

(defn set
  ([graph pathmaps]
   (set graph pathmaps {}))
  ;; TODO: maybe boxed vs unboxed option?
  ([graph pathmaps _]
   (letfn [(set-pathmap [graph path opt pathmap]
             (let [node (get-in graph (into [:graph] opt))
                   graph (cond-> graph
                                 (or (atom? node)
                                     (not (map? node)))
                                 (assoc-in (into [:graph] path) {}))
                   opt (if (ref? node)
                         (:value node)
                         opt)]
               (reduce (fn [graph k]
                         (let [value (clojure.core/get pathmap k)
                               path' (conj path k)
                               opt' (conj opt k)]
                           (cond
                             (ref? value)
                             (assoc-in graph (into [:graph] opt') value)

                             (atom? value)
                             (-> graph
                                 (update :paths conj path')
                                 (assoc-in (into [:graph] opt') value))

                             (map? value)
                             (set-pathmap graph path' opt' value)

                             :else
                             (-> graph
                                 (update :paths conj path')
                                 (assoc-in (into [:graph] opt') (atom value))))))
                       graph
                       (clojure.core/keys pathmap))))]
     (reduce #(set-pathmap %1 [] [] %2)
             {:graph graph :paths []}
             pathmaps))))

(defn set-path-value
  ([graph {:keys [path value]}]
    (set-path-value graph path value))
  ([graph [k & path] value]
    (let [graph (if (or (boxed? graph)
                        (not (map? graph)))
                  {}
                  graph)]
      (if (seq path)
        (assoc graph k (set-path-value (clojure.core/get graph k) path value))
        (assoc graph k (if (boxed? value)
                         value
                         (atom value)))))))

(defrecord GraphDataSource [graph])

;; have to extend type due to get / set names clashing with core ... oops!
(extend-type GraphDataSource
  core/IDataSource
  (get [{:keys [graph]} pathsets]
    (get @graph pathsets {:normalize true
                          :boxed true}))
  (set [{:keys [graph]} pathmaps]
    (with-local-vars [ps nil]
      (let [g (swap! graph (fn [graph]
                             (let [{:keys [graph paths] :as r} (set graph pathmaps)]
                               (var-set ps paths)
                               graph)))
            {:keys [graph]} (get g @ps {:normalize true
                                        :boxed true})]
        {:graph graph
         :paths @ps}))))

(defn as-datasource [graph]
  (GraphDataSource. (clojure.core/atom (:graph (set {} [graph])))))
