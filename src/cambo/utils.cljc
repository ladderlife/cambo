(ns cambo.utils)

#?(:clj
   (defn- cljs-env? [env]
     (boolean (:ns env))))

#?(:clj
   (defmacro if-cljs
     "Return `then` if we are generating cljs code and `else` for Clojure code."
     [then else]
     (if (cljs-env? &env) then else)))
