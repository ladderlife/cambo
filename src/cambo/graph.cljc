(ns cambo.graph
  (:refer-clojure :exclude [get set atom ref keys])
  (:require [cambo.core :refer [branch? boxed? atom atom? ref? keys] :as core]))

(defn get
  ([graph pathsets]
   (get graph pathsets {:normalize false
                        :path-info false
                        :boxed false}))
  ([graph pathsets {:keys [normalize boxed path-info]}]
   (letfn [(get-pathset [node path opt result [ks & pathset :as ps]]
             ;; TODO: can simplify -- have tests / look at pull
             (if-not (branch? node)
               (update result :missing conj (into opt ps))
               (reduce (fn [result k]
                         (if (and (branch? node)
                                  (not (contains? node k)))
                           (update result :missing conj (into (conj opt k) pathset))
                           (let [node (clojure.core/get node k)
                                 path (conj path k)
                                 opt (conj opt k)
                                 result (if (and path-info
                                                 (branch? node))
                                          (assoc-in result
                                                    (into [:graph] (conj path :cambo/path))
                                                    opt)
                                          result)]
                             (if (seq pathset)
                               (cond
                                 (ref? node)
                                 (let [result (if normalize
                                                (assoc-in result (into [:graph] opt) node)
                                                result)
                                       opt (:path node)
                                       result (if path-info
                                                (assoc-in result
                                                          (into [:graph] (conj path :cambo/path))
                                                          opt)
                                                result)
                                       node (get-in graph opt)]
                                   (get-pathset node path opt result pathset))

                                 (branch? node)
                                 (get-pathset node path opt result pathset)

                                 (nil? node)
                                 (assoc-in result (into [:graph] (if normalize opt path))
                                           (if boxed
                                             (atom node)
                                             node))

                                 (core/non-value? node)
                                 result

                                 (atom? node)
                                 (assoc-in result (into [:graph] (if normalize opt path))
                                           (if boxed
                                             node
                                             (:value node)))

                                 :else
                                 (assoc-in result (into [:graph] (if normalize opt path))
                                           (if boxed
                                             (atom node)
                                             node)))
                               (cond
                                 (branch? node) result
                                 (atom? node) (assoc-in result (into [:graph] (if normalize opt path))
                                                        (if boxed
                                                          node
                                                          (:value node)))
                                 (ref? node) (if path-info
                                               (let [opt (:path node)]
                                                 (assoc-in result
                                                           (into [:graph] (conj path :cambo/path))
                                                           opt))
                                               result)
                                 :else (assoc-in result (into [:graph] (if normalize opt path))
                                                 (if boxed
                                                   (atom node)
                                                   node)))))))
                       result
                       (keys ks))))]
     (reduce (partial get-pathset graph [] [])
             {:graph {}
              :missing []}
             pathsets))))

(defn pull
  ([cache query]
   (pull cache query {:normalize false
                      :path-info false
                      :boxed false}))
  ([cache query {:keys [normalize boxed path-info]}]
    ;; overhead of atoms?
   (let [refs (clojure.core/atom [])
         missing (clojure.core/atom [])]
     (letfn [(add-ref! [path query]
               (swap! refs conj (core/prepend-query path query)))
             (add-missing! [path query]
               (swap! missing into (core/prepend-query path query)))
             (set-value [result k node query]
               (cond
                 (branch? node) result
                 (core/non-value? node) result
                 (ref? node) (do
                               (add-ref! (:path node) query)
                               (assoc result k node))
                 :else (assoc result k (cond
                                         (atom? node) (if boxed node (:value node))
                                         :else (if boxed (atom node) node)))))
             (set-path [result k path]
               (if path-info
                 (assoc-in result [k :cambo/path] path)
                 result))
             (inner-query [node result path query]
               (if-not (branch? node)
                 (do (add-missing! path query)
                     result)
                 (reduce (fn [result k]
                           (let [[k query] (if (core/join? k)
                                             (first k)
                                             [k []])]
                             (reduce (fn [result k]
                                       (if-not (contains? node k)
                                         (do (add-missing! (conj path k) query)
                                             result)
                                         (let [node (clojure.core/get node k)
                                               path (conj path k)
                                               result (if (branch? node)
                                                        (set-path result k path)
                                                        result)]
                                           (cond
                                             (and (branch? node)
                                                  (seq query))
                                             (let [inner-result (inner-query node (clojure.core/get result k) path query)]
                                               (if (or (seq inner-result)
                                                       (contains? result k))
                                                 (assoc result k inner-result)
                                                 result))

                                             ;; TODO: revisit refs ;p
                                             (and (ref? node)
                                                  (seq query)
                                                  (not normalize))
                                             (let [ref-path (:path node)
                                                   ;; TODO: we need to assert this ref path too
                                                   ref-result (inner-query (get-in cache ref-path) (clojure.core/get result k) ref-path query)]
                                               (-> result
                                                   (assoc k ref-result)
                                                   (set-path k ref-path)))

                                             :else (set-value result k node query)))))
                                     result
                                     (keys k))))
                         result
                         query)))]
       (loop [result (inner-query cache {} [] query)]
         (let [refs' @refs]
           (reset! refs [])
           (if (seq refs')
             (recur (reduce #(inner-query cache %1 [] %2)
                            result
                            refs'))
             {:graph result
              :missing @missing})))))))

(defn missing-transient
  [graph pathsets]
  (letfn [(missing-pathset [node path missing [ks & pathset :as ps]]
            (if (or (nil? node)
                    (empty? node))
              (conj! missing ps)
              (let []
                (reduce (fn [missing k]
                          (if (and (branch? node)
                                   (not (contains? node k)))
                            (conj! missing (into (conj path k) pathset))
                            (let [node (clojure.core/get node k)
                                  path (conj path k)]
                              (cond
                                (ref? node)
                                (let [path (:path node)
                                      node (get-in graph path)]
                                  (missing-pathset node path missing pathset))

                                (and (branch? node)
                                     (seq pathsets))
                                (missing-pathset node path missing pathset)

                                :else missing))))
                        missing
                        (keys ks)))))]
    (persistent! (reduce (partial missing-pathset graph [])
                         (transient [])
                         pathsets))))

(defn missing
  [graph pathsets]
  (letfn [(missing-pathset [node path [ks & pathset :as ps]]
            (if (or (nil? node)
                    (empty? node))
              [ps]
              (mapcat (fn [k]
                        (if (and (branch? node)
                                 (not (contains? node k)))
                          [(into (conj path k) pathset)]
                          (let [node (clojure.core/get node k)
                                path (conj path k)]
                            (cond
                              (ref? node)
                              (let [path (:path node)
                                    node (get-in graph path)]
                                (missing-pathset node path pathset))

                              (and (branch? node)
                                   (seq pathsets))
                              (missing-pathset node path pathset)

                              :else []))))
                      ;; TODO: potential area of optimization around ranges (diff'ing pre-expansion)
                      (keys ks))))]
    (vec (mapcat (partial missing-pathset graph [])
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
              (if (seq path)
                (update-in graph path dissoc key)
                (dissoc graph key))))]
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
