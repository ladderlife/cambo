(ns cambo.ring
  (:require [cognitect.transit :as transit]
            [cambo.core])
  (:import [java.io ByteArrayOutputStream]
           [cambo.core Range Atom Ref]))

(def read-handlers
  (transit/record-read-handlers Range Atom Ref))

(def write-handlers
  (transit/record-write-handlers Range Atom Ref))

(defn wrap-cambo
  [handler]
  ;; TODO: not sure how much we want to throw vs not throw?
  ;; look at other ring middleware for inspiration :)
  (fn [{:keys [body] :as request}]
    (let [reader (transit/reader body :json {:handlers read-handlers})
          cambo (transit/read reader)
          request (cond-> request
                          cambo (assoc :cambo cambo))
          response (handler request)]
      (if cambo
        (let [out (ByteArrayOutputStream.)
              writer (transit/writer out :json {:handlers write-handlers})
              _ (transit/write writer response)]
          {:status 200
           :headers {"Content-Type" "application/transit+json"}
           :body (.toString out)})
        response))))
