(ns cambo.graph
  (:refer-clojure :exclude [get set atom ref keys])
  (:require [cambo.core :refer [boxed? atom atom? ref? keys] :as core]))

(defn get
  ([graph pathsets]
   (get graph pathsets {:normalize false
                        :path-info false
                        :boxed false}))
  ([graph pathsets {:keys [normalize boxed path-info]}]
   (letfn [(get-pathset [node path opt result [ks & pathset]]
             (reduce (fn [result k]
                       (let [node (clojure.core/get node k)
                             path (conj path k)
                             opt (conj opt k)
                             result (if (and path-info (some? node))
                                      (do (assoc-in result
                                                    (into [:graph] (conj path :cambo/path))
                                                    opt))
                                      result)]
                         (if (seq pathset)
                           (cond
                             (nil? node)
                             (update result :missing conj (into opt pathset))

                             (ref? node)
                             (let [result (if normalize
                                            (assoc-in result (into [:graph] opt) node)
                                            result)
                                   opt (:path node)
                                   result (if path-info
                                            (do (assoc-in result
                                                          (into [:graph] (conj path :cambo/path))
                                                          opt))
                                            result)
                                   node (get-in graph opt)]
                               (get-pathset node path opt result pathset))

                             (atom? node)
                             result

                             :else
                             (get-pathset node path opt result pathset))
                           (cond
                             (atom? node) (assoc-in result (into [:graph] (if normalize opt path))
                                                    (if boxed
                                                      node
                                                      (:value node)))
                             (ref? node) (if path-info
                                           (do (let [opt (:path node)]
                                                 (assoc-in result
                                                           (into [:graph] (conj path :cambo/path))
                                                           opt)))
                                           result)
                             (nil? node) (update result :missing conj opt)
                             :else result))))
                     result
                     (keys ks)))]
     (reduce (partial get-pathset graph [] [])
             {:graph {}
              :missing []}
             pathsets))))

(defn get-value
  [graph path]
  (let [{:keys [graph]} (get graph [path])]
    (get-in graph path)))

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

(defn invalidate
  [graph paths]
  ;; TODO: could remove empty branches post-walk style
  (letfn [(invalidate-path [graph path]
            (let [key (last path)
                  path (butlast path)]
              (update-in graph path dissoc key)))]
    {:graph (reduce invalidate-path graph paths)
     :paths paths}))

(defrecord GraphDataSource [graph])

;; have to extend type due to get / set names clashing with core ... oops!
(extend-type GraphDataSource
  core/IDataSource
  (get [{:keys [graph]} pathsets cb]
    (cb (get @graph pathsets {:normalize true
                              :boxed true}))
    nil)
  (set [{:keys [graph]} pathmaps cb]
    #?(:cljs (let [ps (clojure.core/atom nil)]
               (let [g (swap! graph (fn [graph]
                                      (let [{:keys [graph paths] :as r} (set graph pathmaps)]
                                        (reset! ps paths)
                                        graph)))
                     {:keys [graph]} (get g @ps {:normalize true
                                                 :boxed true})]
                 (cb {:graph graph
                      :paths @ps})))
       :clj (with-local-vars [ps nil]
              (let [g (swap! graph (fn [graph]
                                     (let [{:keys [graph paths] :as r} (set graph pathmaps)]
                                       (var-set ps paths)
                                       graph)))
                    {:keys [graph]} (get g @ps {:normalize true
                                                :boxed true})]
                (cb {:graph graph
                     :paths @ps}))))
    nil)
  (call [_ _ _ _ _]
    (throw (ex-info "not implemented" {:method :call}))))

(defn as-datasource [graph]
  (GraphDataSource. (clojure.core/atom (:graph (set {} [graph])))))
