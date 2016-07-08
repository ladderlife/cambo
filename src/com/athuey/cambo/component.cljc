(ns com.athuey.cambo.component
  (:require [com.athuey.cambo.core :as core]
            [com.athuey.cambo.model :as model]))

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
  (fragments [this]))

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
  (Fragment. (pull (get (fragments comp) name))))

(defn get-fragments [comp]
  (into {} (for [[name fragment] (fragments comp)]
             [name (Fragment. (pull fragment))])))

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
    (for [pathset (.-pathsets fragment)
          :let [pathset (stub-fragments pathset)]]
      (into root pathset))))

(defn query-pathsets [queries container]
  (into [] (for [[name fragment] (get-fragments container)
                 :let [query (get queries name)]
                 pathset (expand-fragment fragment)]
             (into query pathset))))

#?(:cljs (defn create-container
           [component {:keys [initial-variables prepare-variables fragments]
                       :or {initial-variables {} prepare-variables identity}}]
           (let [container (fn container []
                             (this-as this
                               ;; TODO: state!
                               (set! (.-mounted this) true)
                               (.apply js/React.Component this (js-arguments))
                               this))]
             (set! (.-prototype container)
                   (goog.object/clone js/React.Component.prototype))

             (set! (.-contextTypes container) (->context React.PropTypes.object))

             (specify! container
               IFragments
               (fragments [_]
                 (fragments nil)))

             (specify! (.-prototype container)
               Object
               (shouldComponentUpdate [this next-props next-state next-context]
                 ;; TODO: only care about the path of fragment props, other props deep compare
                 (let [get-paths (fn [props]
                                   (into {} (for [[k {:keys [cambo/path] :as v}] props]
                                              [k (if path path v)])))
                       current-props (get-paths (props this))
                       current-state (state this)
                       current-context (context this)
                       next-props (get-paths (get-props next-props))
                       next-state (get-state next-state)
                       next-context (get-context next-context)]
                   (or (not= current-props next-props)
                       (not= current-state next-state)
                       (not= current-context next-context))))
               (initialize [this]
                 )
               (componentWillMount [this]
                 ;; TODO: initialize
                 (let [{:keys [model]} (context this)
                       pathsets (->> (fragments nil)
                                     (mapcat (fn [[name fragment]]
                                               (let [fragment (Fragment. (pull fragment))
                                                     root (get-in (props this) [name :cambo/path])]
                                                 (local-query root fragment))))
                                     (into []))
                       query-data (-> model model/with-path-info (model/get-cache pathsets))
                       subscription (model/subscribe model (fn []
                                                             (let [query-data (-> model model/with-path-info (model/get-cache pathsets))]
                                                               (set-state this {:query-data query-data}))))]
                   (set-state this {:query-data query-data
                                    :subscription subscription})))
               (componentWillReceiveProps [this next-props next-context]
                 ;; TODO: initialize
                 )
               (componentWillUnmount [this]
                 (set! (.-mounted this) false)
                 (let [{:keys [subscription]} (state this)]
                   (when subscription (subscription))))
               ;; fns ....
               ;; - _createQuerySetAndFragmentPointers
               ;; - _runVariables
               ;; - _initialize
               ;; - _cleanup
               ;; - _updateFragmentResolvers
               ;; - _handleFragmentDataUpdate
               ;; - _updateFragmentPointers
               ;; - _getQueryData
               ;; -
               (render [this]
                 (let [query-data (-> this state :query-data)
                       fragment-names (keys (fragments nil))
                       component-props (into {} (for [[name {:keys [cambo/path]}] (select-keys (props this) fragment-names)]
                                                  [name (get-in query-data path)]))
                       component-props (merge (props this)
                                              component-props)]
                   (js/React.createElement component (->props component-props)))))
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