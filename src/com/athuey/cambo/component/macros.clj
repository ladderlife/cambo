(ns com.athuey.cambo.component.macros
  (:require [cljs.core :refer [js-obj specify! this-as js-arguments]]
            [cljs.analyzer :as ana]))

(def lifecycle-sigs
  '{constructor [this props]
    initLocalState [this]
    componentWillReceiveProps [this next-props]
    componentWillUpdate [this next-props next-state]
    componentDidUpdate [this prev-props prev-state]
    componentWillMount [this]
    componentDidMount [this]
    componentWillUnmount [this]
    render [this]})

(defn validate-sig [[name sig :as method]]
  (let [sig' (get lifecycle-sigs name)]
    (assert (= (count sig') (count sig))
            (str "Invalid signature for " name " got " sig ", need " sig'))))

(def reshape-map
  {'componentWillReceiveProps
   (fn [[name [this next-props] & body]]
     `(~name [this# next-props#]
        (let [~this this#
              ~next-props (com.athuey.cambo.component/get-props next-props#)]
          ~@body)))
   'componentWillUpdate
   (fn [[name [this next-props next-state] & body]]
     `(~name [this# next-props# next-state#]
        (let [~this this#
              ~next-props (com.athuey.cambo.component/get-props next-props#)
              ~next-state (com.athuey.cambo.component/get-state next-state#)]
          ~@body)))
   'componentDidUpdate
   (fn [[name [this prev-props prev-state] & body]]
     `(~name [this# prev-props# prev-state#]
        (let [~this this#
              ~prev-props (com.athuey.cambo.component/get-props prev-props#)
              ~prev-state (com.athuey.cambo.component/get-state prev-state#)]
          ~@body)))})

(defn reshape [dt reshape]
  (letfn [(reshape* [x]
            (if (and (sequential? x)
                     (contains? reshape (first x)))
              (let [reshapef (get reshape (first x))]
                (validate-sig x)
                (reshapef x))
              x))]
    (into ['Object]
          (map reshape* dt))))

(defmacro defcomponent [name & forms]
  (let [rname (if &env
                (:name (ana/resolve-var (dissoc &env :locals) name))
                name)
        ctor `(defn ~(with-meta name {:jsdoc ["@constructor"]}) []
                (this-as this#
                         (.apply js/React.Component this# (js-arguments))
                         this#))
        set-react-proto! `(set! (.-prototype ~name)
                                (goog.object/clone js/React.Component.prototype))
        display-name (if &env
                       (str (-> &env :ns :name) "/" name)
                       'js/undefined)]
    `(do
       ~ctor
       ~set-react-proto!

       (specify! (.-prototype ~name) ~@(reshape forms reshape-map))

       (set! (.. ~name -prototype -constructor) ~name)
       (set! (.. ~name -prototype -constructor -displayName) ~display-name)

       (set! (.-cljs$lang$type ~rname) true)
       (set! (.-cljs$lang$ctorStr ~rname) ~(str rname))
       (set! (.-cljs$lang$ctorPrWriter ~rname)
             (fn [this# writer# opt#]
               (cljs.core/-write writer# ~(str rname)))))))

(defmacro defcontainer [name {:keys [component] :as spec}]
  (let [[_ & body] component
        component-name (symbol (str name "*"))
        spec (select-keys spec [:fragments :prepare-variables :initial-variables])]
    `(do
       ;; TODO: create component fn which takes a display name and var name -- then gen-sym shit
       (defcomponent ~component-name ~@body)

       (def ~name (com.athuey.cambo.component/create-container ~component-name ~spec)))))
