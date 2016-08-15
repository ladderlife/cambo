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
  [url request headers cb]
  (let [default-headers {"Content-Type" "application/transit+json"}
        headers (cond-> default-headers
                     (fn? headers) (merge (headers request))
                     (map? headers) (merge headers))
        headers (clj->js headers)]
    (.send XhrIo url
           (fn [_]
             (this-as this
               ;; TODO: handle errors
               (when (= (.getStatus this) 200)
                 (let [response (->edn (.getResponseText this))]
                   (cb response)))))
           "POST"
           (->transit request)
           headers)))

(defrecord HttpDataSource
  [url opts]
  core/IDataSource
  (pull [_ query cb]
    (http-send url {:method :pull :query query} opts cb))
  (set [_ pathmaps cb]
    (http-send url {:method :set :pathmaps pathmaps} opts cb))
  (call [_ path args queries cb]
    (http-send url {:method :call :path path :args args :queries queries} opts cb)))

(defn http-datasource
  ([url]
   (HttpDataSource. url nil))
  ([url opts]
   (HttpDataSource. url opts)))
