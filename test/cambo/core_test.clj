(ns cambo.core-test
  (:refer-clojure :exclude [get set atom ref keys range])
  (:require [cambo.core :refer :all]
            [clojure.test :refer :all]))


;;; KEYS

(deftest range-keys-test
         (is (= [4 5 6 7 8 9]
                (range-keys (range 4 10))))
         (is (= [4]
                (range-keys (range 4 5))))
         (is (= []
                (range-keys (range 4 4))))
         (is (= []
                (range-keys (range 4 3)))))

(deftest keys-test
  (is (= [:foo]
         (keys :foo)))
  (is (= [0 1 :foo]
         (keys [0 1 :foo])))
  (is (= [0 1 2 3 4 5]
         (keys (range 0 6)))))

(deftest keyset-seq-test
  (is (= [:foo]
         (keyset-seq :foo)))
  (is (= [(range 0 5)]
         (keyset-seq (range 0 5))))
  (is (= [:foo "bar" 13]
         (keyset-seq [:foo "bar" 13])))
  (is (= [:foo "bar" (range 0 5) 13]
         (keyset-seq [:foo "bar" (range 0 5) 13]))))

(deftest keyset-test
  (is (= :foo
         (keyset [:foo])))
  (is (= ["bar" (range 13 14) :foo]
         (keyset [:foo "bar" 13]))))

;;; PATHS

;; TODO: a normalize pathset, etc for keyset comparison (make everything a set for order-independent comparison)

(deftest expand-paths-test
  (is (= [[:user 0 :name]
          [:user 0 :age]
          [:user 1 :name]
          [:user 1 :age]]
         (expand-pathsets [[:user (range 0 2) [:name :age]]])))
  (is (= [[:user 0 :name]
          [:user 0 :age]
          [:user 1 :name]
          [:user 1 :age]
          [:video 0 :title]
          [:video 5 :title]
          [:video 6 :title]
          [:video 7 :title]]
         (expand-pathsets [[:user (range 0 2) [:name :age]]
                           [:video [0 (range 5 8)] :title]]))))

(deftest pathsets-test
  (is (= [[:one [:three :two]]
          [:one "four" [(range 0 2)]]]
         (pathsets {:one {:two leaf
                          :three leaf
                          "four" {0 leaf
                                  1 leaf}}}))))

(deftest length-tree-pathsets-test
  (testing "simple path"
    (is (= [[:one :two]]
           (length-tree-pathsets {2 {:one {:two leaf}}}))))
  (testing "complex path"
    (is (= [[:one [:three :two]]]
           (length-tree-pathsets {2 {:one {:two leaf
                                           :three leaf}}}))))
  (testing "simple and complex path"
    (is (= [[:one [:three :two]]
            [:one [(range 0 4)] :summary]]
           (length-tree-pathsets {2 {:one {:two leaf
                                           :three leaf}}
                                  3 {:one {0 {:summary leaf}
                                           1 {:summary leaf}
                                           2 {:summary leaf}
                                           3 {:summary leaf}}}}))))
  (testing "pathmap that has overlapping branch and leaf nodes"
    (is (= [[:lolomo]
            [:lolomo [(range 13 15) :summary]]
            [:lolomo [(range 15 18)] [:summary :rating]]
            [:lolomo [(range 13 15)] :summary]]
           (length-tree-pathsets {1 {:lolomo leaf}
                                  2 {:lolomo {:summary leaf
                                              13 leaf
                                              14 leaf}}
                                  3 {:lolomo {15 {:rating leaf
                                                  :summary leaf}
                                              13 {:summary leaf}
                                              16 {:rating leaf
                                                  :summary leaf}
                                              14 {:summary leaf}
                                              17 {:rating leaf
                                                  :summary leaf}}}})))))

(deftest tree-test
  (testing "simple path"
    (is (= {:one {:two leaf}}
           (tree [[:one :two]]))))
  (testing "complex path"
    (is (= {:one {:two leaf
                  :three leaf}}
           (tree [[:one [:two :three]]]))))
  (testing "set of complex and simple paths"
    (is (= {:one {:two leaf
                  :three leaf
                  0 {:summary leaf}
                  1 {:summary leaf}
                  2 {:summary leaf}
                  3 {:summary leaf}}}
           (tree [[:one [:two :three]]
                  [:one (range 0 4) :summary]])))))

(deftest collapse-test
  (is (= [[:genres 0 :titles [(range 0 2)] [:name :rating]]]
         (collapse [[:genres 0 :titles 0 :name]
                    [:genres 0 :titles 0 :rating]
                    [:genres 0 :titles 1 :name]
                    [:genres 0 :titles 1 :rating]])))
  (is (= [[:genres 0 :titles [(range 0 2)] [:name :rating]]]
         (collapse [[:genres 0 :titles 0 [:name :rating]]
                    [:genres 0 :titles 1 :name]
                    [:genres 0 :titles 1 :rating]]))))

(deftest optimize-test
  (let [cache {:videos-list {3 (ref [:videos 956])
                             5 (ref [:videos 5])
                             :double (ref [:videos-list 3])
                             :short (ref [:videos 5 :more-keys])
                             :inner (ref [:videos-list 3 :inner])}
               :videos {5 (atom "title")
                        6 "a"
                        7 1
                        8 true
                        9 nil}
               :falsey {:string ""
                        :number 0
                        :boolean false
                        :nil nil}}]
    (testing "simple path"
      (is (= [[:videos 956 :summary]]
             (optimize cache [[:videos-list 3 :summary]]))))
    (testing "complex path"
      (is (= [[:videos-list 0 :summary]
              [:videos 956 :summary]]
             (optimize cache [[:videos-list [0 3] :summary]]))))
    (testing "remove found paths"
      (is (= [[:videos-list 0 :summary]
              [:videos 956 :summary]]
             (optimize cache [[:videos-list [0 3 5] :summary]]))))
    (testing "follow double reference"
      (is (= [[:videos 956 :summary]]
             (optimize cache [[:videos-list :double :summary]]))))
    (testing "short circuit ref"
      (is (= []
             (optimize cache [[:videos-list :short :summary]]))))
    (testing "short circuit string"
      (is (= []
             (optimize cache [[:videos 6 :summary]]))))
    (testing "short circuit number"
      (is (= []
             (optimize cache [[:videos 7 :summary]]))))
    (testing "short circuit boolean"
      (is (= []
             (optimize cache [[:videos 8 :summary]]))))
    (testing "short circuit nil"
      (is (= []
             (optimize cache [[:videos 9 :summary]]))))
    (testing "falsey string not missing"
      (is (= []
             (optimize cache [[:falsey :string]]))))
    (testing "falsey number not missing"
      (is (= []
             (optimize cache [[:falsey :number]]))))
    (testing "falsey boolean not missing"
      (is (= []
             (optimize cache [[:falsey :boolean]]))))
    (testing "falsey nil not missing"
      (is (= []
             (optimize cache [[:falsey :nil]]))))
    (testing "inner reference"
      (is (thrown? Exception
                   (optimize cache [[:videos-list :inner :summary]]))))))

(deftest pathmap-paths-test
  (is (= [[:user/by-id 0 :user/name]
          [:user/by-id 0 :user/age]
          [:user/by-id 1 :user/name]
          [:user/by-id 1 :user/age]]
         (pathmap-paths {:user/by-id {0 {:user/name "Erik" :user/age 31}
                                      1 {:user/name "Huey" :user/age 13}}})))
  (is (= [[:user/by-id 0 :user/name]
          [:user/by-id 0 :user/age]
          [:user/by-id 1 :user/name]
          [:user/by-id 1 :user/age]]
         (pathmap-paths {:user/by-id {0 {:user/name (atom {:first "Erik" :last "Petersen"}) :user/age 31}
                                      1 {:user/name "Huey" :user/age 13}}}))))

(deftest pathmap-path-values-test
  (is (= [(path-value [:user/by-id 0 :user/name] "Erik")
          (path-value [:user/by-id 0 :user/age] 31)
          (path-value [:user/by-id 1 :user/name] "Huey")
          (path-value [:user/by-id 1 :user/age] 13)]
         (pathmap-values {:user/by-id {0 {:user/name "Erik" :user/age 31}
                                       1 {:user/name "Huey" :user/age 13}}})))
  (is (= [(path-value [:user/by-id 0 :user/name] (atom {:first "Erik" :last "Petersen"}))
          (path-value [:user/by-id 0 :user/age] 31)
          (path-value [:user/by-id 0 :user/friend] (ref [:user/by-id 1]))
          (path-value [:user/by-id 1 :user/name] "Huey")
          (path-value [:user/by-id 1 :user/age] 13)]
         (pathmap-values {:user/by-id {0 {:user/name (atom {:first "Erik" :last "Petersen"})
                                          :user/age 31
                                          :user/friend (ref [:user/by-id 1])}
                                       1 {:user/name "Huey" :user/age 13}}}))))
