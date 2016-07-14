(ns cambo.http
  (:require [cognitect.transit :as transit]
            [cambo.core :as core])
  (:import [goog.net XhrIo]))

(deftype RecordHandler [t]
  Object
  (tag [this v] t)
  (rep [this v] (into {} v)))

(def write-handlers
  {core/Range (RecordHandler. "cambo.core.Range")
   core/Atom (RecordHandler. "cambo.core.Atom")
   core/Ref (RecordHandler. "cambo.core.Ref")})


(def read-handlers
  {"cambo.core.Range" (fn [{:keys [start end]}]
                        (core/range start end))
   "cambo.core.Ref" (fn [{:keys [path]}]
                      (core/ref path))
   "cambo.core.Atom" (fn [v]
                       (if (contains? v :value)
                         (core/atom (:value v))
                         (core/atom)))})

(defn ->transit
  [edn]
  (let [writer (transit/writer :json {:handlers write-handlers})]
    (transit/write writer edn)))

(defn ->edn
  [transit]
  (let [reader (transit/reader :json {:handlers read-handlers})]
    (transit/read reader transit)))

(defn http-send
  [url request cb]
  (.send XhrIo url
         (fn [_]
           (this-as this
             ;; TODO: handle errors
             (when (= (.getStatus this) 200)
               (let [response (->edn (.getResponseText this))]
                 (cb response)))))
         "POST" (->transit request)
         #js {"Content-Type" "application/transit+json"}))

(defrecord HttpDataSource
  [url]
  core/IDataSource
  (get [_ pathsets cb]
    (http-send url {:method :get :pathsets pathsets} cb))
  (set [_ pathmaps cb]
    (http-send url {:method :set :pathmaps pathmaps} cb))
  (call [_ path args queries cb]
    (http-send url {:method :call :path path :args args :queries queries} cb)))

(defn http-datasource
  [url]
  (HttpDataSource. url))
