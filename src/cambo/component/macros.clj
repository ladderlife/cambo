(ns cambo.component.macros
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
              ~next-props (cambo.component/get-props next-props#)]
          ~@body)))
   'componentWillUpdate
   (fn [[name [this next-props next-state] & body]]
     `(~name [this# next-props# next-state#]
        (let [~this this#
              ~next-props (cambo.component/get-props next-props#)
              ~next-state (cambo.component/get-state next-state#)]
          ~@body)))
   'componentDidUpdate
   (fn [[name [this prev-props prev-state] & body]]
     `(~name [this# prev-props# prev-state#]
        (let [~this this#
              ~prev-props (cambo.component/get-props prev-props#)
              ~prev-state (cambo.component/get-state prev-state#)]
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
                ;; copied from om.next -- no idea
                (:name (ana/resolve-var (dissoc &env :locals) name))
                name)
        ctor `(defn ~(with-meta name {:jsdoc ["@constructor"]}) []
                (this-as this#
                         (.apply js/React.Component this# (js-arguments))
                         this#))
        set-react-proto! `(set! (.-prototype ~name)
                                (goog.object/clone js/React.Component.prototype))
        {:keys [display-name]} (meta name)
        display-name (or display-name
                         (if &env
                           ;; copied from om.next -- no idea
                           (str (-> &env :ns :name) "/" name)
                           'js/undefined))]
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

(defn collect-container [forms]
  (letfn [(split-on-spec [forms]
            (split-with (complement keyword?) forms))
          (split-on-component [forms]
            (split-with (complement seq?) forms))]
    (loop [forms forms specs {} component []]
      (if-let [form (first forms)]
        (cond
          (keyword? form)
          (let [[specs' remaining] (split-on-component forms)
                specs' (into {} (map vec (partition 2 specs')))]
            (recur remaining (merge specs specs') component))

          (seq? form)
          (let [[component' remaining] (split-on-spec forms)]
            (recur remaining specs (into component component'))))
        {:component component :specs specs}))))

(defmacro defcontainer [name & forms]
  (let [{:keys [specs component]} (collect-container forms)
        component-name (with-meta (gensym (str name "_")) {:anonymous true
                                                           :display-name (str name "*")})
        display-name (if &env
                       (str (-> &env :ns :name) "/" name)
                       'js/undefined)]
    `(do
       (defcomponent ~component-name ~@component)

       (def ~name (let [container# (cambo.component/create-container ~component-name ~specs)]
                    (set! (.-displayName container#) ~display-name)
                    container#)))))

(defmacro profile
  ([name args]
   `(let [start# (js/window.performance.now)]
      (fn []
        (let [duration# (- (js/window.performance.now) start#)]
          (when cambo.component/*profile*
            (swap! cambo.component/*profile* update ~name (fnil conj []) [~args duration#]))))))
  ([name args & body]
   `(let [start# (js/window.performance.now)
          result# (do ~@body)
          duration# (- (js/window.performance.now) start#)]
      (when cambo.component/*profile*
        (swap! cambo.component/*profile* update ~name (fnil conj []) [~args duration#]))
      result#)))
