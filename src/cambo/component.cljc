(ns cambo.component
  #?(:cljs (:require-macros [cambo.component.macros]))
  (:require [cambo.core :as core]
            [cambo.model :as model]
            #?(:clj [cambo.component.macros])
            #?(:cljs [cljsjs.react])))

;;; REACT

(def props-key "cambo$props")

(defn get-props
  [props]
  (aget props props-key))

(defn props
  ([comp]
   (get-props (.-props comp)))
  ([comp key]
   (get (get-props (.-props comp)) key)))

#?(:cljs (declare container?))

#?(:cljs (defn ->props
           [{:keys [key ref] :as props}]
           (let [js-props (js-obj props-key props)]
             ;; TODO: nicer way to do this?
             ;; pull key from props into component react base props
             (when key (aset js-props "key" key))
             ;; we pass the component as the ref -- not the container itself
             ;; if we ever need the container itself, can revisit this
             ;; TODO: assert ref is a fn, not string
             (when ref (aset js-props "ref" #(ref (if (container? %)
                                                    (some-> % .-component)
                                                    %))))
             js-props)))

(defn get-children
  [props]
  (.-children props))

(defn children
  [comp]
  (get-children (.-props comp)))

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

;;; HELPERS

(defn range-seq
  [elements]
  (->> elements
       (filter (comp integer? first))
       (sort-by first)))

;;; FRAGMENTS

(defprotocol IFragments
  (fragment-names [this])
  (fragment [this name]))

(defn fragments? [x]
  (satisfies? IFragments x))

(defprotocol IFragment
  (query [this]))

(defn fragment? [x] (satisfies? IFragment x))

(deftype LazyFragment [delay]
  IFragment
  (query [_] @delay))

(defprotocol IRecursive
  (depth [this]))

(defn recursive? [x] (satisfies? IRecursive x))

(deftype RecursiveFragment [fragment depth]
  IFragment
  (query [_]
    (query fragment))
  IRecursive
  (depth [_] depth))

(defn get-fragment
  ([fragments name]
   (fragment fragments name))
  ([fragments name depth]
   (RecursiveFragment. (get-fragment fragments name) depth)))

;; NOTE:
;; - this shouldn't be shared -- should be per fragment instance
;; - doesn't need to be dynamic anymore ... but whatever!
(def ^:dynamic *depth* nil)

(defn full-fragment
  [fragment]
  (letfn [(walk-query [q]
            (into []
                  (mapcat (fn [entry]
                            (cond
                              (core/key? entry) [entry]
                              (map? entry) (let [[k q] (first entry)
                                                 q (walk-query q)]
                                             (when (seq q)
                                               [{k q}]))
                              (fragment? entry) (if (recursive? entry)
                                                  (binding [*depth* (dec (or *depth* (depth entry)))]
                                                    (when (>= *depth* 0)
                                                      (walk-query (query entry))))
                                                  (walk-query (query entry))))))
                  q))]
    (walk-query (query fragment))))

(defn full-query
  [path fragment]
  (core/prepend-query path (full-fragment fragment)))

(defn local-fragment
  [fragment]
  (letfn [(walk-query [q]
            (into []
                  (comp (map (fn [entry]
                               (cond
                                 (core/key? entry) entry
                                 (map? entry) (let [[k q] (first entry)
                                                    q (walk-query q)]
                                                (if (seq q)
                                                  {k q}
                                                  k))
                                 (fragment? entry) nil)))
                        (filter some?))
                  q))]
    (walk-query (query fragment))))

(defn local-query
  [path fragment]
  (core/prepend-query path (local-fragment fragment)))

;;; CONTAINER

(defprotocol IContainer
  (get-container-fragment [this name vars]))

(defn container-fragment [f vars]
  (LazyFragment. (delay (core/eval-query (f vars)))))

(defn mock?
  [_]
  false)

(defn get-path
  [props name]
  (get-in props [name :cambo/path]))

(defprotocol IDisposable
  (dispose [this]))

;; TODO: get rid of this -- can be a function?
#?(:cljs (deftype ContainerSubscription
           [^:mutable model ^:mutable query ^:mutable sub cb]
           IDisposable
           (dispose [this]
             (.unsubscribe this)
             nil)

           Object
           (update [this new-model new-query]
             (when (or (not= model new-model)
                       (not= query new-query))
               (.unsubscribe this)
               (set! model new-model)
               (set! query new-query)
               (.subscribe this))
             this)
           (subscribe [this]
             (set! sub (model/subscribe model query cb))
             this)
           (unsubscribe [this]
             (when sub
               (sub))
             this)))

#?(:cljs (defn container-subscription [model query cb]
           (.subscribe (ContainerSubscription. model query nil cb))))

(defn create-fragment-pointers
  [container props vars]
  (into {} (for [name (fragment-names container)
                 :let [root (get-path props name)]
                 :when root
                 :let [fragment (get-container-fragment container name vars)
                       query (local-query root fragment)]]
             [name {:root root
                    :query query}])))

(def variables-key "cambo$variables")

#?(:cljs (defn get-variables
           [props]
           (aget props variables-key)))

#?(:cljs (defn variables*
           [this]
           (get-variables (.-props this))))

#?(:cljs (defn variables
           [this]
           (get (variables* this) :variables)))

#?(:cljs (defn set-variables
           ([this variables]
            (set-variables this variables nil))
           ([this variables cb]
            (let [{:keys [run]} (variables* this)]
              (run variables cb false)))))

#?(:cljs (defn force-fetch
           ([this]
            (force-fetch this nil nil))
           ([this variables]
            (force-fetch this variables nil))
           ([this variables cb]
            (let [{:keys [run]} (variables* this)]
              (run variables cb true)))))

#?(:cljs (defn ^:boolean container?
           [x]
           (if-not (nil? x)
             (true? (. x -cambo$isContainer))
             false)))

#?(:cljs (defn create-container*
           [{:keys [initial-variables prepare-variables fragments]} component]
           (let [fragment-names (into #{} (keys fragments))
                 fragment-cache (volatile! {})
                 container (fn container []
                             (this-as this
                               (.apply js/React.Component this (js-arguments))
                               (set! (.-pending this) nil)
                               (set! (.-mounted this) true)
                               (set! (.-state this)
                                     (->state {:query-data nil
                                               :variables nil
                                               :cambo/variables {:run (fn [variables cb force?]
                                                                        (.run-variables this variables cb force?))
                                                                 :set (fn [pathmaps cb]
                                                                        (let [{:keys [model]} (context this)]
                                                                          (model/set model pathmaps cb)))
                                                                 :call (fn [path args queries cb]
                                                                         (let [{:keys [model]} (context this)]
                                                                           (model/call model path args queries cb)))}}))
                               this))]
             (set! (.-prototype container)
                   (goog.object/clone js/React.Component.prototype))

             (set! (.-contextTypes container) (->context React.PropTypes.object))

             (set! (.. container -prototype -cambo$isContainer) true)

             (specify! container
               IFragments
               (fragment-names [_] fragment-names)
               (fragment [this name]
                 (get-container-fragment this name (prepare-variables initial-variables nil)))
               IContainer
               (get-container-fragment [_ name vars]
                 (if-let [v (get @fragment-cache [name vars])]
                   v
                   (let [fragment-fn (get fragments name)
                         fragment (container-fragment fragment-fn vars)]
                     (vswap! fragment-cache assoc [name vars] fragment)
                     fragment))))

             (specify! (.-prototype container)
               Object

               (run-variables [this partial-vars cb force?]
                 (let [{:keys [route model]} (context this)
                       {:keys [variables]} (state this)
                       variables (get (.-pending this) :variables variables)
                       variables (merge variables partial-vars)
                       next-variables (prepare-variables variables route)
                       pointers (create-fragment-pointers container (props this) next-variables)
                       query (into [] (mapcat :query (vals pointers)))
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
                     (model/force model query on-readystate)
                     (model/prime model query on-readystate))
                   (set! (.-pending this) {:variables variables})))

               (initialize [this {:keys [cambo/variables]} props {:keys [route] :as context} vars]
                 (let [next-vars (prepare-variables vars route)]
                   (.update-fragment-pointers this props next-vars)
                   (.update-subscription this context)
                   {:query-data (.get-query-data this props context)
                    :variables vars
                    :cambo/variables (assoc variables :variables next-vars)}))

               (update-fragment-pointers [this props vars]
                 (set! (.-fragment-pointers this) (create-fragment-pointers container props vars)))

               (update-subscription [this {:keys [model]}]
                 (let [pointers (.-fragment-pointers this)
                       query (into [] (mapcat :query (vals pointers)))
                       subscription (if-let [subscription (.-subscription this)]
                                      (.update subscription model query)
                                      (container-subscription model query (fn []
                                                                            (.handle-fragment-data-update this))))]
                   (set! (.-subscription this) subscription)))

               (get-query-data [this props {:keys [model]}]
                 (let [pointers (.-fragment-pointers this)
                       query (into [] (mapcat :query (vals pointers)))
                       graph (-> model model/with-path-info (model/pull-cache query))
                       query-data (into {} (map (fn [[name {:keys [root]}]]
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
                 (if (not= (get-children next-props)
                           (children this))
                   true
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
                                                   variables-key variables
                                                   "ref" #(set! (.-component this) %))
                                           children))))

             container)))

#?(:cljs (do
           (defn ^{:jsdoc ["@constructor"]} StaticContainer []
             (this-as this
               (.apply js/React.Component this (js-arguments))
               this))

           (set! (.-prototype StaticContainer)
                 (goog.object/clone js/React.Component.prototype))

           (specify! (.-prototype StaticContainer)
             Object
             (shouldComponentUpdate [this next-props next-state]
               (not (not (aget next-props "shouldUpdate"))))
             (render [this]
               (when-let [child (.. this -props -children)]
                 (React.Children.only child))))))

(defn build-query
  [queries container]
  (into []
        (mapcat (fn [[name path]]
                  (let [fragment (get-fragment container name)
                        query (full-query path fragment)]
                    query)))
        queries))

#?(:cljs (do
           (defn ^{:jsdoc ["@constructor"]} Renderer []
             (this-as this
               ;; TODO: state!
               (set! (.-mounted this) true)
               (set! (.-state this) (->state {:readystate nil}))
               (.apply js/React.Component this (js-arguments))
               this))

           (set! (.-prototype Renderer)
                 (goog.object/clone js/React.Component.prototype))

           ;; this is not in extern ...
           ;(set! (.-childContextTypes Renderer) (->context React.PropTypes.object))

           (aset Renderer "childContextTypes" (->context React.PropTypes.object))

           (specify! (.-prototype Renderer)
             Object
             (run-queries [this {:keys [container model queries force]}]
               (let [query (build-query queries container)
                     on-readystate (fn [readystate]
                                     (if (.-mounted this)
                                       (set-state this {:readystate readystate})
                                       (.handle-readystate-change this readystate)))]
                 (if force
                   (model/force model query on-readystate)
                   (model/prime model query on-readystate))))
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
                     container-props (dissoc (props this) :container :model :queries :force)
                     container-props (into container-props
                                           (for [[name path] queries]
                                             [name {:cambo/path path}]))
                     ;; TODO: support render prop -- handle js/undefined response
                     container-element (when readystate
                                         (js/React.createElement container
                                                                 (->props container-props)
                                                                 (children this)))]
                 (js/React.createElement StaticContainer
                                         #js {"shouldUpdate" (some? container-element)}
                                         container-element))))))

#?(:cljs (defn renderer [props & children]
           (apply js/React.createElement Renderer (->props props) children)))

(defn factory [cls]
  #?(:cljs (fn [props & children]
             (apply React.createElement cls (->props props) children))
     :clj (fn [& args])))

#?(:cljs (defn set-model
           [this prop-key pathmap]
           (let [{:keys [set]} (variables* this)
                 path (get-path (props this) prop-key)]
             (set [(assoc-in {} path pathmap)] (fn [_])))))

#?(:cljs (defn call-model
           ([this path]
            (call-model this path nil))
           ([this path args]
            (call-model this path args {}))
           ([this path args queries]
            (call-model this path args queries (fn [_])))
           ([this path args queries cb]
            (let [{:keys [call]} (variables* this)]
              (call path args queries cb)))))

;;; MACROS

#?(:clj
   (defn- cljs-env? [env]
     (boolean (:ns env))))

#?(:clj
   (defmacro if-cljs
     "Return `then` if we are generating cljs code and `else` for Clojure code."
     [then else]
     (if (cljs-env? &env) then else)))

#?(:clj
   (defmacro defcomponent [name & forms]
     `(if-cljs
        (cambo.component.macros/defcomponent-cljs ~name ~@forms)
        (cambo.component.macros/defcomponent-clj ~name ~@forms))))

#?(:clj
   (defmacro defcontainer [name & forms]
     `(if-cljs
        (cambo.component.macros/defcontainer-cljs ~name ~@forms)
        (cambo.component.macros/defcontainer-clj ~name ~@forms))))
