(ns cambo.profile
  (:require [cambo.utils :as utils]))

(def ^:dynamic *profile* true)

(defn p-cljs [tag body]
  (if *profile*
    (let [tag (str tag)]
      `(let [measure# ~tag
             mark# (str (gensym ~tag))
             _# (js/performance.mark mark#)
             start# (js/performance.now)
             result# (do ~@body)
             duration# (- (js/performance.now) start#)]
         (when (>= duration# 0.1)
           (js/performance.measure measure# mark#))
         (js/performance.clearMeasures measure#)
         (js/performance.clearMarks mark#)
         result#))
    `(do ~@body)))

#?(:clj
   (defmacro p [tag & body]
     `(utils/if-cljs
        ~(p-cljs tag body)
        (do ~@body))))

(defn pa-cljs [tag done body]
  (if *profile*
    (let [tag (str tag)]
      `(let [measure# ~tag
             mark# (str (gensym ~tag))
             _# (js/performance.mark mark#)
             start# (js/performance.now)
             done# (fn []
                     (let [duration# (- (js/performance.now) start#)]
                       (when (>= duration# 0.1)
                         (js/performance.measure measure# mark#))
                       (js/performance.clearMeasures measure#)
                       (js/performance.clearMarks mark#)))]
         (let [~done done#]
           (do ~@body))))
    `(do ~@body)))

#?(:clj
   (defmacro pa [tag [done] & body]
     (utils/if-cljs
       (pa-cljs tag done body)
       `(do ~@body))))

(comment


  (binding [*profile* true]
    (macroexpand-1 '(p :foo
                       (println "foo")
                       (model/pull [:query]))))

  (binding [*profile* true]
    (macroexpand-1 '(pa :foo [xxx]
                        (println "foo")
                        (model/pull [:query] (fn [_]
                                               (xxx))))))

  )
