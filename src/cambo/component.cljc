(ns cambo.component
  (:require [cambo.core :as core]
            [cambo.model :as model]
            #?(:cljs [cljsjs.react])))

(def props-key "cambo$props")

(defn get-props
  [props]
  (aget props props-key))

(defn props
  [comp]
  (get-props (.-props comp)))

#?(:cljs (defn ->props
           [props]
           (js-obj props-key props)))

(def state-key "cambo$state")

(defn get-state
  [state]
  (when state
    (aget state state-key)))

(defn state
  [comp]
  (get-state (.-state comp)))

#?(:cljs (defn ->state
           [state]
           (js-obj state-key state)))

#?(:cljs (defn set-state
           [comp fn-or-map]
           (let [state-fn (if (fn? fn-or-map)
                            (fn [state props]
                              (let [cambo-state (get-state state)
                                    cambo-props (get-props props)
                                    next-state (fn-or-map cambo-state cambo-props)]
                                (->state (merge cambo-state next-state))))
                            (fn [state props]
                              (let [cambo-state (get-state state)]
                                (->state (merge cambo-state fn-or-map)))))]
             (.setState comp state-fn))))

(def context-key "cambo$context")

(defn get-context
  [context]
  (aget context context-key))

#?(:cljs (defn ->context
           [context]
           (js-obj context-key context)))

(defn context
  [comp]
  (get-context (.-context comp)))

(defprotocol IFragments
  (fragment-names [this])
  (fragment [this name]))

(deftype Fragment [pathsets])

(defn fragment? [x]
  (instance? Fragment x))

(defn pull [query]
  (let [leafs (into [] (remove map? query))
        paths (for [join (filter map? query)
                    :let [[key query] (first join)]
                    paths (pull query)]
                (into [key] paths))]
    (cond-> (into [] paths)
            (seq leafs) (conj [leafs]))))

(defn get-fragment [comp name]
  (fragment comp name))

(defn expand-fragment
  [fragment]
  (letfn [(expand-pathset [pathset]
            (let [path (into [] (butlast pathset))
                  x (last pathset)]
              (cond
                (fragment? x) (for [pathset (expand-fragment fragment)]
                                (into path pathset))
                (core/key? x) [(conj path x)]
                (vector? x) (let [keys (into [] (filter core/key? x))
                                  fragments (filter fragment? x)]
                              (cond-> []
                                      (seq keys) (conj (conj path keys))
                                      (seq fragments) (into (for [fragment fragments
                                                                  pathset (expand-fragment fragment)]
                                                              (into path pathset))))))))]
    (into [] (mapcat expand-pathset) (.-pathsets fragment))))

(defn fragment-query [root fragment]
  (into [] (for [pathset (expand-fragment fragment)]
             (into root pathset))))

;(defn remove-fragments
;  [fragment]
;  (letfn [(remove-from-pathset [pathset]
;            (let [x (last pathset)]
;              (cond
;                (fragment? x) nil
;                (core/key? x) pathset
;                (vector? x) (let [keys (into [] (filter core/key? x))]
;                              (if (empty? keys)
;                                nil
;                                (let [path (into [] (butlast pathset))]
;                                  (conj path keys)))))))]
;    (into [] (keep remove-from-pathset (.-pathsets fragment)))))
;
;(defn local-query [root fragment]
;  (for [pathset (remove-fragments fragment)]
;    (into root pathset)))

(defn local-query [root fragment]
  (letfn [(stub-fragments [pathset]
            (let [path (into [] (butlast pathset))
                  x (last pathset)]
              (cond
                (fragment? x) path
                (core/key? x) pathset
                (vector? x) (let [keys (into [] (filter core/key? x))]
                              (if (empty? keys)
                                path
                                (conj path keys))))))]
    (into [] (for [pathset (.-pathsets fragment)
                   :let [pathset (stub-fragments pathset)]]
               (into root pathset)))))

(defn query-pathsets [queries container]
  (into [] (for [name (fragment-names container)
                 :let [fragment (get-fragment container name)
                       query (get queries name)
                       pathsets (expand-fragment fragment)]
                 pathset pathsets]
             (into query pathset))))

(defn mock?
  [_]
  false)

(defn default-prepare-variables [vars _]
  vars)

(defn get-container-fragment
  [fragments name vars]
  (when-let [fragment (get fragments name)]
    (let [query (if (fn? fragment)
                  (fragment vars)
                  fragment)
          pathsets (pull query)]
      (Fragment. pathsets))))

(defn get-path
  [props name]
  (get-in props [name :cambo/path]))

(defprotocol IDisposable
  (dispose [this]))

#?(:cljs (deftype ContainerSubscription
           [cb]
           IDisposable
           (dispose [this]
             (.unsubscribe this)
             nil)

           Object
           (update [this model pathsets]
             (let [current-model (.-model this)
                   current-pathsets (.-pathsets this)]
               (when (or (not= model current-model)
                         (not= pathsets current-pathsets))
                 (.unsubscribe this)
                 (set! (.-model this) model)
                 (set! (.-pathsets this) pathsets)
                 (.subscribe this)))
             this)
           (subscribe [this]
             (let [model (.-model this)
                   pathsets (.-pathsets this)
                   sub (model/subscribe model pathsets cb)]
               (set! (.-sub this) sub))
             this)
           (unsubscribe [this]
             (when-let [sub (.-sub this)]
               (sub))
             this)))

#?(:cljs (defn container-subscription [model pathsets cb]
           (let [sub (ContainerSubscription. cb)]
             (.update sub model pathsets)
             sub)))

(defn create-fragment-pointers
  [fragments props vars]
  (into {} (for [name (keys fragments)
                 :let [root (get-path props name)]
                 :when root
                 :let [fragment (get-container-fragment fragments name vars)
                       pathsets (local-query root fragment)]]
             [name {:root root
                    :pathsets pathsets}])))

(def variables-key "cambo$variables")

(defn get-variables
  [props]
  (aget props variables-key))

(defn variables*
  [this]
  (get-variables (.-props this)))

(defn variables
  [this]
  (get (variables* this) :variables))

(defn set-variables
  ([this variables]
   (set-variables this variables nil))
  ([this variables cb]
   (let [{:keys [run]} (variables* this)]
     (run variables cb false))))

(defn force-fetch
  ([this]
   (force-fetch this nil nil))
  ([this variables]
   (force-fetch this variables nil))
  ([this variables cb]
   (let [{:keys [run]} (variables* this)]
     (run variables cb true))))

#?(:cljs (defn create-container
           [component {:keys [initial-variables prepare-variables fragments]
                       :or {initial-variables {} prepare-variables default-prepare-variables}}]
           (let [container (fn container []
                             (this-as this
                               (.apply js/React.Component this (js-arguments))
                               (set! (.-pending this) nil)
                               (set! (.-mounted this) true)
                               (set! (.-state this)
                                     (->state {:query-data nil
                                               :variables nil
                                               :cambo/variables {:run (fn [variables cb force?]
                                                                        (.run-variables this variables cb force?))}}))
                               this))]
             (set! (.-prototype container)
                   (goog.object/clone js/React.Component.prototype))

             (set! (.-contextTypes container) (->context React.PropTypes.object))

             (specify! container
               IFragments
               (fragment-names [_]
                 (into #{} (keys fragments)))
               (fragment [_ name]
                 ;; TODO: figure out how routes work for external `get-fragment` calls -- it is not nil in relay
                 (get-container-fragment fragments name (prepare-variables initial-variables nil))))

             (specify! (.-prototype container)
               Object

               (run-variables [this partial-vars cb force?]
                 (let [{:keys [route model]} (context this)
                       {:keys [variables]} (state this)
                       variables (get (.-pending this) :variables variables)
                       variables (merge variables partial-vars)
                       next-variables (prepare-variables variables route)
                       pointers (create-fragment-pointers fragments (props this) next-variables)
                       pathsets (into [] (mapcat :pathsets (vals pointers)))
                       on-readystate (fn [ready]
                                       (when ready
                                         (set! (.-pending this) nil)
                                         (set! (.-fragment-pointers this) pointers)
                                         (.update-subscription this (context this))
                                         (when (.-mounted this)
                                           (set-state this (fn [{:keys [cambo/variables]}]
                                                             {:query-data (.get-query-data this (props this) (context this))
                                                              :variables variables
                                                              :cambo/variables (assoc variables :variables next-variables)}))))
                                       (when cb
                                         (cb ready)))]
                   (if force?
                     (model/force model pathsets on-readystate)
                     (model/prime model pathsets on-readystate))
                   (set! (.-pending this) {:variables variables})))

               (initialize [this {:keys [cambo/variables]} props {:keys [route] :as context} vars]
                 (let [next-vars (prepare-variables vars route)]
                   (.update-fragment-pointers this props next-vars)
                   (.update-subscription this context)
                   {:query-data (.get-query-data this props context)
                    :variables vars
                    :cambo/variables (assoc variables :variables next-vars)}))

               (update-fragment-pointers [this props vars]
                 (set! (.-fragment-pointers this) (create-fragment-pointers fragments props vars)))

               (update-subscription [this {:keys [model]}]
                 (let [pointers (.-fragment-pointers this)
                       pathsets (into [] (mapcat :pathsets (vals pointers)))
                       subscription (if-let [subscription (.-subscription this)]
                                      (.update subscription model pathsets)
                                      (container-subscription model pathsets (fn []
                                                                               (.handle-fragment-data-update this))))]
                   (set! (.-subscription this) subscription)))

               (get-query-data [this props {:keys [model]}]
                 (let [pointers (.-fragment-pointers this)
                       pathsets (mapcat :pathsets (vals pointers))
                       graph (-> model model/with-path-info (model/get-cache pathsets))
                       query-data (into {} (map (fn [[name {:keys [root]}]]
                                                     ;; how would this happen?
                                                     ;:when (get props name)
                                                  [name (get-in graph root)])
                                                pointers))]
                   query-data))

               (handle-fragment-data-update [this]
                 (when (.-mounted this)
                   (set-state this {:query-data (.get-query-data this (props this) (context this))})))

               (componentWillMount [this]
                 (when-not (mock? this)
                   (set-state this (fn [state]
                                     (.initialize this state (props this) (context this) initial-variables)))))

               (componentWillReceiveProps [this next-props next-context]
                 (when-not (mock? this)
                   (set-state this (fn [{:keys [variables] :as state}]
                                     (.initialize this state (get-props next-props) (get-context next-context) variables)))))

               (componentWillUnmount [this]
                 (set! (.-mounted this) false)
                 (when-let [sub (.-subscription this)]
                   (dispose sub)))

               (shouldComponentUpdate [this next-props next-state next-context]
                 (if (not= (.-children next-props)
                           (.. this -props -children))
                   false
                   (let [update-props (fn [props]
                                        (reduce (fn [props key]
                                                  (let [path (get-path props key)]
                                                    (assoc props key path)))
                                                props
                                                (keys fragments)))
                         current-props (update-props (props this))
                         current-state (state this)
                         current-context (context this)
                         next-props (update-props (get-props next-props))
                         next-state (get-state next-state)
                         next-context (get-context next-context)]
                     (or (not= current-props next-props)
                         (not= current-state next-state)
                         (not= current-context next-context)))))

               (render [this]
                 (let [{:keys [query-data cambo/variables]} (state this)
                       component-props (merge (props this)
                                              query-data)
                       children (.. this -props -children)]
                   (js/React.createElement component
                                           (js-obj props-key component-props
                                                   variables-key variables)
                                           children))))

             container)))

#?(:cljs (do
           (defn StaticContainer []
             (this-as this
               (.apply js/React.Component this (js-arguments))
               this))

           (set! (.-prototype StaticContainer)
                 (goog.object/clone js/React.Component.prototype))

           (specify! (.-prototype StaticContainer)
             Object
             (shouldComponentUpdate [this next-props next-state]
               (not (not (.-shouldUpdate next-props))))
             (render [this]
               (when-let [child (.. this -props -children)]
                 (React.Children.only child))))))

#?(:cljs (do
           (defn Renderer []
             (this-as this
               ;; TODO: state!
               (set! (.-mounted this) true)
               (set! (.-state this) (->state {:readystate nil}))
               (.apply js/React.Component this (js-arguments))
               this))

           (set! (.-prototype Renderer)
                 (goog.object/clone js/React.Component.prototype))

           (set! (.-childContextTypes Renderer) (->context React.PropTypes.object))

           (specify! (.-prototype Renderer)
             Object
             (run-queries [this {:keys [container model queries force]}]
               (let [pathsets (query-pathsets queries container)
                     on-readystate (fn [readystate]
                                     (if (.-mounted this)
                                       (set-state this {:readystate readystate})
                                       (.handle-readystate-change this readystate)))]
                 (if force
                   (model/force model pathsets on-readystate)
                   (model/prime model pathsets on-readystate))))
             (handle-readystate-change [this readystate]
               (when-let [on-readystate-change (:on-readystate-change (props this))]
                 (on-readystate-change readystate)))
             (getChildContext [this]
               (let [{:keys [model]} (props this)]
                 (->context {:model model})))
             (componentDidMount [this]
               (.run-queries this (props this)))
             (componentWillReceiveProps [this next-props]
               (let [renderer-props [:model :queries :container :force]]
                 (when-not (= (select-keys (props this) renderer-props)
                              (select-keys (get-props next-props) renderer-props))
                   (set-state this {:readystate nil})
                   (.run-queries this (get-props next-props)))))
             (componentDidUpdate [this prev-props prev-state]
               (let [readystate (:readystate (state this))
                     prev-readystate (:readystate (get-state prev-state))]
                 (when-not (= readystate prev-readystate)
                   (.handle-readystate-change this readystate))))
             (componentWillUnmount [this]
               (set! (.-mounted this) false))
             (render [this]
               (let [{:keys [container queries]} (props this)
                     {:keys [readystate]} (state this)
                     container-props (into {} (for [[name query] queries]
                                                [name {:cambo/path query}]))
                     ;; TODO: support render prop -- handle js/undefined response
                     children (when readystate
                                (js/React.createElement container (->props container-props)))]
                 (js/React.createElement StaticContainer #js {"shouldUpdate" (some? children)} children))))))

#?(:cljs (defn renderer [props]
           (js/React.createElement Renderer (->props props))))

#?(:cljs (defn factory [cls]
           (fn [props & children]
             (apply React.createElement cls (->props props) children))))