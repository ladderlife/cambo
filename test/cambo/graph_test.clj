(ns cambo.graph-test
  (:refer-clojure :exclude [get set range atom ref])
  (:require [cambo.graph :refer :all]
            [cambo.core :as core :refer [range atom ref]]
            [clojure.test :refer :all]))

(def cache
  {:users {0 (ref [:user/by-id 123])
           1 nil
           2 (atom nil)
           3 (atom)}
   :user/by-id {123 {:user/name (atom "Erik")
                     :user/age (atom 31)
                     :user/gender :gender/male
                     :user/complex-name (atom {:first "Erik"
                                               :last "Petersen"})}}})

(deftest get-basic-test
  (let [get #(get %1 %2 {:normalize true
                         :path-info false
                         :boxed false})]
    (testing "unoptimized get"
      (is (= {:graph {:user/by-id {123 {:user/name "Erik"
                                        :user/age 31}}}
              :missing []}
             (get cache [[:user/by-id 123 [:user/name :user/age]]]))))
    (testing "getting an non-boxed value"
      (is (= {:graph {:user/by-id {123 {:user/gender :gender/male}}}
              :missing []}
             (get cache [[:user/by-id 123 :user/gender]]))))
    (testing "getting a branch"
      (is (= {:graph {}
              :missing []}
             (get cache [[:user/by-id 123]]))))
    (testing "getting into an atom"
      (is (= {:graph {:user/by-id {123 {:user/name "Erik"}}}
              :missing []}
             (get cache [[:user/by-id 123 :user/name :name/first]]))))
    (testing "getting into non-boxed value"
      (is (= {:graph {:user/by-id {123 {:user/gender :gender/male}}}
              :missing []}
             (get cache [[:user/by-id 123 :user/gender :gender/string]]))))
    (testing "getting a ref"
      (is (= {:graph {:users {0 (ref [:user/by-id 123])}
                      :user/by-id {123 {:user/name "Erik"
                                        :user/age 31}}}
              :missing []}
             (get cache [[:users 0 [:user/name :user/age]]]))))
    (testing "getting a nil value"
      (is (= {:graph {:users {1 nil}}
              :missing []}
             (get cache [[:users 1 [:user/name :user/age]]]))))
    (testing "getting a nil atom"
      (is (= {:graph {:users {2 nil}}
              :missing []}
             (get cache [[:users 2 [:user/name :user/age]]]))))
    (testing "getting an empty atom"
      (is (= {:graph {}
              :missing []}
             (get cache [[:users 3 [:user/name :user/age]]]))))
    (testing "getting a missing path"
      (is (= {:graph {}
              :missing [[:user/by-id 123 :user/height]]}
             (get cache [[:user/by-id 123 :user/height]]))))
    (testing "getting a deep missing path"
      (is (= {:graph {}
              :missing [[:user/by-id 456 :user/email [:email/address
                                                      :email/domain]]]}
             (get cache [[:user/by-id 456 :user/email [:email/address
                                                       :email/domain]]]))))
    (testing "getting a missing path with ref"
      (is (= {:graph {}
              :missing [[:users 4 [:user/name :user/age]]]}
             (get cache [[:users 4 [:user/name :user/age]]]))))
    (testing "getting a deep missing path with ref"
      (is (= {:graph {}
              :missing [[:users 4 :user/email [:email/address
                                               :email/domain]]]}
             (get cache [[:users 4 :user/email [:email/address
                                                :email/domain]]]))))
    (testing "getting a partial missing path"
      (is (= {:graph {:user/by-id {123 {:user/name "Erik"
                                        :user/age 31}}}
              :missing [[:user/by-id 123 :user/height]]}
             (get cache [[:user/by-id 123 [:user/name
                                           :user/age
                                           :user/height]]]))))
    (testing "getting a partial missing path with ref"
      (is (= {:graph {:users {0 (ref [:user/by-id 123])}
                      :user/by-id {123 {:user/name "Erik"
                                        :user/age 31}}}
              :missing [[:user/by-id 123 :user/height]]}
             (get cache [[:users 0 [:user/name
                                    :user/age
                                    :user/height]]]))))
    (testing "getting a range"
      (is (= {:graph {:users {0 (ref [:user/by-id 123])
                              1 nil
                              2 nil}
                      :user/by-id {123 {:user/name "Erik"
                                        :user/age 31}}}
              :missing [[:users 4 [:user/name :user/age]]]}
             (get cache [[:users (range 0 5) [:user/name :user/age]]]))))))

(deftest get-denormalized-with-paths-test
  (let [get #(get %1 %2 {:normalize false
                         :path-info true
                         :boxed false})]
    (testing "unoptimized get"
      (is (= {:graph {:user/by-id {123 {:user/name "Erik"
                                        :user/age 31
                                        :cambo/path [:user/by-id 123]}
                                   :cambo/path [:user/by-id]}}
              :missing []}
             (get cache [[:user/by-id 123 [:user/name :user/age]]]))))
    (testing "getting an non-boxed value"
      (is (= {:graph {:user/by-id {123 {:user/gender :gender/male
                                        :cambo/path [:user/by-id 123]}
                                   :cambo/path [:user/by-id]}}
              :missing []}
             (get cache [[:user/by-id 123 :user/gender]]))))
    (testing "getting a branch"
      (is (= {:graph {:user/by-id {123 {:cambo/path [:user/by-id 123]}
                                   :cambo/path [:user/by-id]}}
              :missing []}
             (get cache [[:user/by-id 123]]))))
    (testing "getting into an atom"
      (is (= {:graph {:user/by-id {123 {:user/name "Erik"
                                        :cambo/path [:user/by-id 123]}
                                   :cambo/path [:user/by-id]}}
              :missing []}
             (get cache [[:user/by-id 123 :user/name :name/first]]))))
    (testing "getting into non-boxed value"
      (is (= {:graph {:user/by-id {123 {:user/gender :gender/male
                                        :cambo/path [:user/by-id 123]}
                                   :cambo/path [:user/by-id]}}
              :missing []}
             (get cache [[:user/by-id 123 :user/gender :gender/string]]))))
    (testing "getting a ref"
      (is (= {:graph {:users {0 {:user/name "Erik"
                                 :user/age 31
                                 :cambo/path [:user/by-id 123]}
                              :cambo/path [:users]}}
              :missing []}
             (get cache [[:users 0 [:user/name :user/age]]]))))
    (testing "getting a nil value"
      (is (= {:graph {:users {1 nil
                              :cambo/path [:users]}}
              :missing []}
             (get cache [[:users 1 [:user/name :user/age]]]))))
    (testing "getting a nil atom"
      (is (= {:graph {:users {2 nil
                              :cambo/path [:users]}}
              :missing []}
             (get cache [[:users 2 [:user/name :user/age]]]))))
    (testing "getting an empty atom"
      (is (= {:graph {:users {:cambo/path [:users]}}
              :missing []}
             (get cache [[:users 3 [:user/name :user/age]]]))))
    (testing "getting a missing path"
      (is (= {:graph {:user/by-id {123 {:cambo/path [:user/by-id 123]}
                                   :cambo/path [:user/by-id]}}
              :missing [[:user/by-id 123 :user/height]]}
             (get cache [[:user/by-id 123 :user/height]]))))
    (testing "getting a deep missing path"
      (is (= {:graph {:user/by-id {:cambo/path [:user/by-id]}}
              :missing [[:user/by-id 456 :user/email [:email/address
                                                      :email/domain]]]}
             (get cache [[:user/by-id 456 :user/email [:email/address
                                                       :email/domain]]]))))
    (testing "getting a missing path with ref"
      (is (= {:graph {:users {:cambo/path [:users]}}
              :missing [[:users 4 [:user/name :user/age]]]}
             (get cache [[:users 4 [:user/name :user/age]]]))))
    (testing "getting a deep missing path with ref"
      (is (= {:graph {:users {:cambo/path [:users]}}
              :missing [[:users 4 :user/email [:email/address
                                               :email/domain]]]}
             (get cache [[:users 4 :user/email [:email/address
                                                :email/domain]]]))))
    (testing "getting a partial missing path"
      (is (= {:graph {:user/by-id {123 {:user/name "Erik"
                                        :user/age 31
                                        :cambo/path [:user/by-id 123]}
                                   :cambo/path [:user/by-id]}}
              :missing [[:user/by-id 123 :user/height]]}
             (get cache [[:user/by-id 123 [:user/name
                                           :user/age
                                           :user/height]]]))))
    (testing "getting a partial missing path with ref"
      (is (= {:graph {:users {0 {:user/name "Erik"
                                 :user/age 31
                                 :cambo/path [:user/by-id 123]}
                              :cambo/path [:users]}}
              :missing [[:user/by-id 123 :user/height]]}
             (get cache [[:users 0 [:user/name
                                    :user/age
                                    :user/height]]]))))
    (testing "getting a range"
      (is (= {:graph {:users {0 {:user/name "Erik"
                                 :user/age 31
                                 :cambo/path [:user/by-id 123]}
                              1 nil
                              2 nil
                              :cambo/path [:users]}}
              :missing [[:users 4 [:user/name :user/age]]]}
             (get cache [[:users (range 0 5) [:user/name :user/age]]]))))
    (testing "getting a leaf ref range"
      #_(is (= {:graph {:users {0 {:cambo/path [:user/by-id 123]}
                              1 nil
                              2 nil
                              :cambo/path [:users]}}
              :missing [[:users 4]]}
             (get cache [[:users (range 0 5)]]))))))

(deftest pull-basic-test
  (let [pull #(pull %1 %2 {:normalize true
                           :path-info false
                           :boxed false})]
    (testing "unoptimized get"
      (is (= {:graph {:user/by-id {123 {:user/name "Erik"
                                        :user/age 31}}}
              :missing []}
             (pull cache [{:user/by-id [{123 [:user/name
                                              :user/age]}]}]))))
    (testing "getting an non-boxed value"
      (is (= {:graph {:user/by-id {123 {:user/gender :gender/male}}}
              :missing []}
             (pull cache [{:user/by-id [{123 [:user/gender]}]}]))))
    (testing "getting a branch"
      (is (= {:graph {}
              :missing []}
             (pull cache [{:user/by-id [123]}]))))
    (testing "getting into an atom"
      (is (= {:graph {:user/by-id {123 {:user/name "Erik"}}}
              :missing []}
             (pull cache [{:user/by-id [{123 [{:user/name [:name/first]}]}]}]))))
    (testing "getting into non-boxed value"
      (is (= {:graph {:user/by-id {123 {:user/gender :gender/male}}}
              :missing []}
             (pull cache [{:user/by-id [{123 [{:user/gender [:gender/string]}]}]}]))))
    (testing "getting a ref"
      (is (= {:graph {:users {0 (ref [:user/by-id 123])}
                      :user/by-id {123 {:user/name "Erik"
                                        :user/age 31}}}
              :missing []}
             (pull cache [{:users [{0 [:user/name
                                       :user/age]}]}]))))
    (testing "getting a nil value"
      (is (= {:graph {:users {1 nil}}
              :missing []}
             (pull cache [{:users [{1 [:user/name
                                       :user/age]}]}]))))
    (testing "getting a nil atom"
      (is (= {:graph {:users {2 nil}}
              :missing []}
             (pull cache [{:users [{2 [:user/name
                                       :user/age]}]}]))))
    (testing "getting an empty atom"
      (is (= {:graph {}
              :missing []}
             (pull cache [{:users [{3 [:user/name
                                       :user/age]}]}]))))
    (testing "getting a missing path"
      (is (= {:graph {}
              :missing [{:user/by-id [{123 [:user/height]}]}]}
             (pull cache [{:user/by-id [{123 [:user/height]}]}]))))
    (testing "getting a deep missing path"
      (is (= {:graph {}
              :missing [{:user/by-id [{456 [{:user/email [:email/address
                                                          :email/domain]}]}]}]}
             (pull cache [{:user/by-id [{456 [{:user/email [:email/address
                                                            :email/domain]}]}]}]))))
    (testing "getting a missing path with ref"
      (is (= {:graph {}
              :missing [{:users [{4 [:user/name
                                     :user/age]}]}]}
             (pull cache [{:users [{4 [:user/name
                                       :user/age]}]}]))))
    (testing "getting a deep missing path with ref"
      (is (= {:graph {}
              :missing [{:users [{4 [{:user/email [:email/address
                                                   :email/domain]}]}]}]}
             (pull cache [{:users [{4 [{:user/email [:email/address
                                                     :email/domain]}]}]}]))))
    (testing "getting a partial missing path"
      (is (= {:graph {:user/by-id {123 {:user/name "Erik"
                                        :user/age 31}}}
              :missing [{:user/by-id [{123 [:user/height]}]}]}
             (pull cache [{:user/by-id [{123 [:user/name
                                              :user/age
                                              :user/height]}]}]))))
    (testing "getting a partial missing path with ref"
      (is (= {:graph {:users {0 (ref [:user/by-id 123])}
                      :user/by-id {123 {:user/name "Erik"
                                        :user/age 31}}}
              :missing [{:user/by-id [{123 [:user/height]}]}]}
             (pull cache [{:users [{0 [:user/name
                                       :user/age
                                       :user/height]}]}]))))
    (testing "getting a range"
      (is (= {:graph {:users {0 (ref [:user/by-id 123])
                              1 nil
                              2 nil}
                      :user/by-id {123 {:user/name "Erik"
                                        :user/age 31}}}
              :missing [{:users [{4 [:user/name
                                     :user/age]}]}]}
             (pull cache [{:users [{(range 0 5) [:user/name
                                                 :user/age]}]}]))))))

(deftest pull-denormalized-with-paths-test
  (let [pull #(pull %1 %2 {:normalize false
                           :path-info true
                           :boxed false})]
    (testing "unoptimized get"
      (is (= {:graph {:user/by-id {123 {:user/name "Erik"
                                        :user/age 31
                                        :cambo/path [:user/by-id 123]}
                                   :cambo/path [:user/by-id]}}
              :missing []}
             (pull cache [{:user/by-id [{123 [:user/name
                                              :user/age]}]}]))))
    (testing "getting an non-boxed value"
      (is (= {:graph {:user/by-id {123 {:user/gender :gender/male
                                        :cambo/path [:user/by-id 123]}
                                   :cambo/path [:user/by-id]}}
              :missing []}
             (pull cache [{:user/by-id [{123 [:user/gender]}]}]))))
    (testing "getting a branch"
      (is (= {:graph {:user/by-id {123 {:cambo/path [:user/by-id 123]}
                                   :cambo/path [:user/by-id]}}
              :missing []}
             (pull cache [{:user/by-id [123]}]))))
    (testing "getting into an atom"
      (is (= {:graph {:user/by-id {123 {:user/name "Erik"
                                        :cambo/path [:user/by-id 123]}
                                   :cambo/path [:user/by-id]}}
              :missing []}
             (pull cache [{:user/by-id [{123 [{:user/name [:name/first]}]}]}]))))
    (testing "getting into non-boxed value"
      (is (= {:graph {:user/by-id {123 {:user/gender :gender/male
                                        :cambo/path [:user/by-id 123]}
                                   :cambo/path [:user/by-id]}}
              :missing []}
             (pull cache [{:user/by-id [{123 [{:user/gender [:gender/string]}]}]}]))))
    (testing "getting a ref"
      (is (= {:graph {:users {0 {:user/name "Erik"
                                 :user/age 31
                                 :cambo/path [:user/by-id 123]}
                              :cambo/path [:users]}}
              :missing []}
             (pull cache [{:users [{0 [:user/name
                                       :user/age]}]}]))))
    (testing "getting a nil value"
      (is (= {:graph {:users {1 nil
                              :cambo/path [:users]}}
              :missing []}
             (pull cache [{:users [{1 [:user/name
                                       :user/age]}]}]))))
    (testing "getting a nil atom"
      (is (= {:graph {:users {2 nil
                              :cambo/path [:users]}}
              :missing []}
             (pull cache [{:users [{2 [:user/name
                                       :user/age]}]}]))))
    (testing "getting an empty atom"
      (is (= {:graph {:users {:cambo/path [:users]}}
              :missing []}
             (pull cache [{:users [{3 [:user/name
                                       :user/age]}]}]))))
    (testing "getting a missing path"
      (is (= {:graph {:user/by-id {123 {:cambo/path [:user/by-id 123]}
                                   :cambo/path [:user/by-id]}}
              :missing [{:user/by-id [{123 [:user/height]}]}]}
             (pull cache [{:user/by-id [{123 [:user/height]}]}]))))
    (testing "getting a deep missing path"
      (is (= {:graph {:user/by-id {:cambo/path [:user/by-id]}}
              :missing [{:user/by-id [{456 [{:user/email [:email/address
                                                          :email/domain]}]}]}]}
             (pull cache [{:user/by-id [{456 [{:user/email [:email/address
                                                            :email/domain]}]}]}]))))
    (testing "getting a missing path with ref"
      (is (= {:graph {:users {:cambo/path [:users]}}
              :missing [{:users [{4 [:user/name
                                     :user/age]}]}]}
             (pull cache [{:users [{4 [:user/name
                                       :user/age]}]}]))))
    (testing "getting a deep missing path with ref"
      (is (= {:graph {:users {:cambo/path [:users]}}
              :missing [{:users [{4 [{:user/email [:email/address
                                                   :email/domain]}]}]}]}
             (pull cache [{:users [{4 [{:user/email [:email/address
                                                     :email/domain]}]}]}]))))
    (testing "getting a partial missing path"
      (is (= {:graph {:user/by-id {123 {:user/name "Erik"
                                        :user/age 31
                                        :cambo/path [:user/by-id 123]}
                                   :cambo/path [:user/by-id]}}
              :missing [{:user/by-id [{123 [:user/height]}]}]}
             (pull cache [{:user/by-id [{123 [:user/name
                                              :user/age
                                              :user/height]}]}]))))
    (testing "getting a partial missing path with ref"
      (is (= {:graph {:users {0 {:user/name "Erik"
                                 :user/age 31
                                 :cambo/path [:user/by-id 123]}
                              :cambo/path [:users]}}
              :missing [{:user/by-id [{123 [:user/height]}]}]}
             (pull cache [{:users [{0 [:user/name
                                       :user/age
                                       :user/height]}]}]))))
    (testing "getting a range"
      (is (= {:graph {:users {0 {:user/name "Erik"
                                 :user/age 31
                                 :cambo/path [:user/by-id 123]}
                              1 nil
                              2 nil
                              :cambo/path [:users]}}
              :missing [{:users [{4 [:user/name
                                     :user/age]}]}]}
             (pull cache [{:users [{(range 0 5) [:user/name
                                                 :user/age]}]}]))))
    (testing "getting a leaf ref range"
      (is (= {:graph {:users {0 {:cambo/path [:user/by-id 123]}
                              1 nil
                              2 nil
                              :cambo/path [:users]}}
              :missing [{:users [4]}]}
             (pull cache [{:users [(range 0 5)]}]))))))

(deftest missing-test
  (testing "unoptimized get"
    (is (= []
           (missing cache [[:user/by-id 123 [:user/name :user/age]]]))))
  (testing "getting an non-boxed value"
    (is (= []
           (missing cache [[:user/by-id 123 :user/gender]]))))
  (testing "getting a branch"
    (is (= []
           (missing cache [[:user/by-id 123]]))))
  (testing "getting into an atom"
    (is (= []
           (missing cache [[:user/by-id 123 :user/name :name/first]]))))
  (testing "getting into non-boxed value"
    (is (= []
           (missing cache [[:user/by-id 123 :user/gender :gender/string]]))))
  (testing "getting a ref"
    (is (= []
           (missing cache [[:users 0 [:user/name :user/age]]]))))
  (testing "getting a nil value"
    (is (= []
           (missing cache [[:users 1 [:user/name :user/age]]]))))
  (testing "getting a nil atom"
    (is (= []
           (missing cache [[:users 2 [:user/name :user/age]]]))))
  (testing "getting an empty atom"
    (is (= []
           (missing cache [[:users 3 [:user/name :user/age]]]))))
  (testing "getting a missing path"
    (is (= [[:user/by-id 123 :user/height]]
           (missing cache [[:user/by-id 123 :user/height]]))))
  (testing "getting a deep missing path"
    (is (= [[:user/by-id 456 :user/email [:email/address
                                          :email/domain]]]
           (missing cache [[:user/by-id 456 :user/email [:email/address
                                                         :email/domain]]]))))
  (testing "getting a missing path with ref"
    (is (= [[:users 4 [:user/name :user/age]]]
           (missing cache [[:users 4 [:user/name :user/age]]]))))
  (testing "getting a deep missing path with ref"
    (is (= [[:users 4 :user/email [:email/address
                                   :email/domain]]]
           (missing cache [[:users 4 :user/email [:email/address
                                                  :email/domain]]]))))
  (testing "getting a partial missing path"
    (is (= [[:user/by-id 123 :user/height]]
           (missing cache [[:user/by-id 123 [:user/name
                                             :user/age
                                             :user/height]]]))))
  (testing "getting a partial missing path with ref"
    (is (= [[:user/by-id 123 :user/height]]
           (missing cache [[:users 0 [:user/name
                                      :user/age
                                      :user/height]]]))))
  (testing "getting a range"
    (is (= [[:users 4 [:user/name :user/age]]]
           (missing cache [[:users (range 0 5) [:user/name :user/age]]])))))

(deftest missing-transient-test
  (testing "unoptimized get"
    (is (= []
           (missing-transient cache [[:user/by-id 123 [:user/name :user/age]]]))))
  (testing "getting an non-boxed value"
    (is (= []
           (missing-transient cache [[:user/by-id 123 :user/gender]]))))
  (testing "getting a branch"
    (is (= []
           (missing-transient cache [[:user/by-id 123]]))))
  (testing "getting into an atom"
    (is (= []
           (missing-transient cache [[:user/by-id 123 :user/name :name/first]]))))
  (testing "getting into non-boxed value"
    (is (= []
           (missing-transient cache [[:user/by-id 123 :user/gender :gender/string]]))))
  (testing "getting a ref"
    (is (= []
           (missing-transient cache [[:users 0 [:user/name :user/age]]]))))
  (testing "getting a nil value"
    (is (= []
           (missing-transient cache [[:users 1 [:user/name :user/age]]]))))
  (testing "getting a nil atom"
    (is (= []
           (missing-transient cache [[:users 2 [:user/name :user/age]]]))))
  (testing "getting an empty atom"
    (is (= []
           (missing-transient cache [[:users 3 [:user/name :user/age]]]))))
  (testing "getting a missing path"
    (is (= [[:user/by-id 123 :user/height]]
           (missing-transient cache [[:user/by-id 123 :user/height]]))))
  (testing "getting a deep missing path"
    (is (= [[:user/by-id 456 :user/email [:email/address
                                          :email/domain]]]
           (missing-transient cache [[:user/by-id 456 :user/email [:email/address
                                                                   :email/domain]]]))))
  (testing "getting a missing path with ref"
    (is (= [[:users 4 [:user/name :user/age]]]
           (missing-transient cache [[:users 4 [:user/name :user/age]]]))))
  (testing "getting a deep missing path with ref"
    (is (= [[:users 4 :user/email [:email/address
                                   :email/domain]]]
           (missing-transient cache [[:users 4 :user/email [:email/address
                                                            :email/domain]]]))))
  (testing "getting a partial missing path"
    (is (= [[:user/by-id 123 :user/height]]
           (missing-transient cache [[:user/by-id 123 [:user/name
                                                       :user/age
                                                       :user/height]]]))))
  (testing "getting a partial missing path with ref"
    (is (= [[:user/by-id 123 :user/height]]
           (missing-transient cache [[:users 0 [:user/name
                                                :user/age
                                                :user/height]]]))))
  (testing "getting a range"
    (is (= [[:users 4 [:user/name :user/age]]]
           (missing-transient cache [[:users (range 0 5) [:user/name :user/age]]])))))

(comment


  (require '[criterium.core :as bench])

  (bench/quick-bench
    (missing-transient cache [[:users (range 0 100) [:user/name
                                                     :user/age
                                                     :user/height]]]))

  (bench/quick-bench
    (missing cache [[:users (range 0 100) [:user/name
                                           :user/age
                                           :user/height]]]))

  (bench/quick-bench
    (:missing (get cache [[:users (range 0 100) [:user/name
                                                 :user/age
                                                 :user/height]]])))


  (= (missing-transient cache [[:users (range 0 100) [:user/name
                                                      :user/age
                                                      :user/height]]])
     (missing cache [[:users (range 0 100) [:user/name
                                            :user/age
                                            :user/height]]])
     (:missing (get cache [[:users (range 0 100) [:user/name
                                                  :user/age
                                                  :user/height]]])))

  (bench/quick-bench
    (pull cache [{:users [{(range 0 5) [:user/name
                                        :user/age]}]}]
          {:normalize true
           :path-info false
           :boxed false}))

  ;; 16us w/ no range check
  ;; 27us w/ cond range check
  ;; 32us w/ true range check
  ;; 16us w/ false range check
  (bench/quick-bench
    (get cache [[:users (range 0 5) [:user/name :user/age]]]
         {:normalize true
          :path-info false
          :boxed false}))

  ;; 98us w/ no range check
  ;; 25us w/ cond range check
  ;; 32us w/ true range check
  ;; 90us w/ false range check
  (bench/quick-bench
    (get cache [[:users (range 0 100) [:user/name :user/age]]]
         {:normalize true
          :path-info false
          :boxed false}))


  (= (:graph (pull cache [{:users [{(range 0 5) [:user/name
                                                 :user/age]}]}]
                   {:normalize true
                    :path-info false
                    :boxed false}))
     (:graph (get cache [[:users (range 0 5) [:user/name :user/age]]]
                  {:normalize true
                   :path-info false
                   :boxed false})))

  )