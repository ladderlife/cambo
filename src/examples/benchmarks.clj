(ns examples.benchmarks
  (:require [examples.queries :as queries]
            [cambo.graph :as graph]
            [criterium.core :as bench]))

;; benchmark!

(comment

  ;;; APPLICATION SCREEN

  (doseq [pathsets queries/application-queries]
    (bench/quick-bench
      (graph/get queries/full-uwb
                 pathsets
                 {:normalize false
                  :path-info true
                  :boxed false})))

  (let [pathsets (nth queries/application-queries 5)]
    (bench/quick-bench
      (graph/get queries/full-uwb
                 pathsets
                 {:normalize false
                  :path-info true
                  :boxed false})))

  (let [pathsets (reduce into queries/application-queries)]
    (bench/quick-bench
      (graph/get queries/full-uwb
                 pathsets
                 {:normalize false
                  :path-info true
                  :boxed false})))

  (let [pathsets (reduce into queries/application-queries)]
    (bench/quick-bench
      (graph/missing queries/full-uwb pathsets)))

  ;;; QUESTIONNAIRE SCREEN



  )