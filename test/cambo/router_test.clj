(ns cambo.router-test
  (:refer-clojure :exclude [get set range atom ref])
  (:require [cambo.router :refer :all]
            [cambo.core :as core :refer [path-value range atom ref]]
            [clojure.test :refer :all]))

(deftest route-hash-test
         (is (= [:foo 0 :cambo.router/i "bar"]
                (route-hash [:foo 0 INTEGERS "bar"])))
         (is (= (route-hash [:foo 0 INTEGERS "bar"])
                (route-hash [:foo 0 RANGES "bar"]))))

(deftest expand-routeset-test
  (is (= [[:user/by-id 0 :name]
          [:user/by-id 0 :age]
          [:user/by-id 1 :name]
          [:user/by-id 1 :age]
          [:users 0 :name]
          [:users 0 :age]
          [:users 1 :name]
          [:users 1 :age]]
         (expand-routeset [[:user/by-id :users] [0 1] [:name :age]]))))

(deftest route-tree-test
  (let [route1 {:route [:users/by-id RANGES [:name :age]]
                :get true}
        route2 {:route [:users]
                :get true}]
    (is (= {:users/by-id {RANGES {:name {match-key {:get route1}}
                                 :age {match-key {:get route1}}}}
            :users {match-key {:get route2}}}
           (clojure.walk/postwalk #(cond-> % (map? %) (dissoc id-key))
                                  (route-tree [route1 route2])))))
  (is (thrown? Exception
               (let [route1 {:route [:users/by-id RANGES [:name :age]]
                             :get true}
                     route2 {:route [:users/by-id INTEGERS [:name :age]]
                             :get true}]
                 (route-tree [route1 route2])))))

(deftest strip-keys-test
  (is (= [[:a] []]
         (strip KEYS :a)))
  (is (= [["a"] []]
         (strip KEYS ["a"])))
  (is (= [[1] []]
         (strip KEYS 1)))
  (is (= [[1] []]
         (strip KEYS [1])))
  (is (= [[1 2 3] []]
         (strip KEYS [1 2 3])))
  (is (= [[1 :a 2 :b 3 :c :d] []]
         (strip KEYS [1 :a 2 :b 3 :c :d])))
  (is (= [[(core/range 0 10)] []]
         (strip KEYS (core/range 0 10))))
  (is (= [[(core/range 0 10) (core/range 5 15)] []]
         (strip KEYS [(core/range 0 10) (core/range 5 15)])))
  (is (= [[(core/range 0 10) :a (core/range 5 15) :b 1 2] []]
         (strip KEYS [(core/range 0 10) :a (core/range 5 15) :b 1 2]))))

(deftest strip-integers-test
  (is (= [[1] []]
         (strip INTEGERS 1)))
  (is (= [[1] []]
         (strip INTEGERS [1])))
  (is (= [[] [:a]]
         (strip INTEGERS :a)))
  (is (= [[]  ["a"]]
         (strip INTEGERS ["a"])))
  (is (= [[1 2 3] []]
         (strip INTEGERS [1 2 3])))
  (is (= [[1 2 3] [:a :b :c :d]]
         (strip INTEGERS [1 :a 2 :b 3 :c :d])))
  (is (= [[(core/range 0 10)] []]
         (strip INTEGERS (core/range 0 10))))
  (is (= [[(core/range 0 10) (core/range 5 15)] []]
         (strip INTEGERS [(core/range 0 10) (core/range 5 15)])))
  (is (= [[(core/range 0 10) (core/range 5 15) 1 2 ] [:a :b]]
         (strip INTEGERS [(core/range 0 10) :a (core/range 5 15) :b 1 2]))))

(deftest strip-ranges-test
  (is (= [[1] []]
         (strip RANGES 1)))
  (is (= [[1] []]
         (strip RANGES [1])))
  (is (= [[] [:a]]
         (strip RANGES :a)))
  (is (= [[]  ["a"]]
         (strip RANGES ["a"])))
  (is (= [[1 2 3] []]
         (strip RANGES [1 2 3])))
  (is (= [[1 2 3] [:a :b :c :d]]
         (strip RANGES [1 :a 2 :b 3 :c :d])))
  (is (= [[(core/range 0 10)] []]
         (strip RANGES (core/range 0 10))))
  (is (= [[(core/range 0 10) (core/range 5 15)] []]
         (strip RANGES [(core/range 0 10) (core/range 5 15)])))
  (is (= [[(core/range 0 10) (core/range 5 15) 1 2 ] [:a :b]]
         (strip RANGES [(core/range 0 10) :a (core/range 5 15) :b 1 2]))))

(deftest strip-keyword-test
  (is (= [[:a] []]
         (strip :a :a)))
  (is (= [[:a] []]
         (strip :a [:a])))
  (is (= [[] ["a"]]
         (strip :a "a")))
  (is (= [[] ["a" 1 (core/range 1 10)]]
         (strip :a ["a" 1 (core/range 1 10)]))))

(deftest strip-long-test
  (is (= [[] [(core/range 5 10)]]
         (strip 1 (core/range 5 10))))
  (is (= [[] [(core/range 5 10)]]
         (strip 15 (core/range 5 10))))
  (is (= [[0] [(core/range 1 10)]]
         (strip 0 (core/range 0 10))))
  (is (= [[] [(core/range 0 10)]]
         (strip 10 (core/range 0 10))))
  (is (= [[1] [(core/range 0 1) (core/range 2 10)]]
         (strip 1 (core/range 0 10))))
  (is (= [[5] [(core/range 0 5) (core/range 6 10)]]
         (strip 5 (core/range 0 10))))
  (is (= [[8] [(core/range 0 8) (core/range 9 10)]]
         (strip 8 (core/range 0 10))))
  (is (= [[9] [(core/range 0 9)]]
         (strip 9 (core/range 0 10)))))

(deftest strip-vector-test
  (is (= [[1 8] [(core/range 0 1) (core/range 2 8) (core/range 9 10)]]
         (strip [1 8] (core/range 0 10))))
  (is (= [[1 8] [2 3 4 5 6 7 9 10]]
         (strip [1 8] [1 2 3 4 5 6 7 8 9 10])))
  (is (= [[8] ["a" :b]]
         (strip [1 8] ["a" :b 8]))))

(deftest strip-path-test
  (testing "simple keys"
    (is (= [[:a :b :c]
            []]
           (strip-path [:a :b :c]
                         [:a :b :c]))))
  (testing "simple keys with route token"
    (is (= [[:a :b :c]
            []]
           (strip-path [:a KEYS :c]
                       [:a :b :c]))))
  (testing "path with array args"
    (is (= [[:a [:b :d] :c]
            []]
           (strip-path [:a KEYS :c]
                       [:a [:b :d] :c]))))
  (testing "path with range args"
    (is (= [[:a (range 0 6) :c]
            []]
           (strip-path [:a RANGES :c]
                       [:a (range 0 6) :c]))))
  (testing "path with array keys"
    (is (= [[:a :b :c]
            [[:a :d :c]]]
           (strip-path [:a :b :c]
                       [:a [:b :d] :c]))))
  (testing "path with range"
    (is (= [[:a 1 :c]
            [[:a (range 0 1) :c]
             [:a (range 2 6) :c]]]
           (strip-path [:a 1 :c]
                       [:a (range 0 6) :c]))))
  (testing "path with array range"
    (is (= [[:a 1 :c]
            [[:a (range 0 1) :c]
             [:a (range 2 3) :c]
             [:a (range 5 6) :c]]]
           (strip-path [:a 1 :c]
                       [:a [(range 0 3) (range 5 6)] :c]))))
  (testing "path with complement partial match"
    (is (= [[:a :c :e]
            [[:b [:c :d] [:e :f]]
             [:a :d [:e :f]]
             [:a :c :f]]]
           (strip-path [:a :c :e]
                       [[:a :b] [:c :d] [:e :f]])))))

(deftest get-test
  (let [noop (fn [& _])
        video-routes {:summary (fn [f]
                                 [{:route [:videos :summary]
                                   :get (fn [path _]
                                          (when f (f path))
                                          [(path-value [:videos :summary] (atom 75))])}])}
        precedence-router (fn [on-title on-rating]
                            (router [{:route [:videos INTEGERS :title]
                                      :get (fn [[_ ids _ :as path] _]
                                             (when on-title (on-title path))
                                             (for [id ids]
                                               (path-value [:videos id :title] (str "title " id))))}
                                     {:route [:videos INTEGERS :rating]
                                      :get (fn [[_ ids _ :as path] _]
                                             (when on-rating (on-rating path))
                                             (for [id ids]
                                               (path-value [:videos id :rating] (str "rating " id))))}
                                     {:route [:lists KEYS INTEGERS]
                                      :get (fn [[_ ids idxs] _]
                                             (for [id ids
                                                   idx idxs]
                                               (path-value [:lists id idx] (ref [:videos idx]))))}]))]
    (testing "simple route"
      (let [router (router ((video-routes :summary) noop))]
        (is (= {:videos {:summary (atom 75)}}
               (:graph (get router [[:videos :summary]]))))))
    (testing "should validate that optimizedPathSets strips out already found data."
      (let [calls (clojure.core/atom 0)
            router (router [{:route [:lists KEYS]
                             :get (fn [[_ ids] _]
                                    (for [id ids]
                                      (if (= 0 id)
                                        (path-value [:lists id] (ref [:two :be 956]))
                                        (path-value [:lists id] (ref [:lists 0])))))}
                            {:route [:two :be INTEGERS :summary]
                             :get (fn [[_ _ ids] _]
                                    (for [id ids]
                                      (do
                                        (swap! calls inc)
                                        (path-value [:two :be id :summary] "hello world"))))}])
            result (get router [[:lists [0 1] :summary]])]
        (is (= {:lists {0 (ref [:two :be 956])
                        1 (ref [:lists 0])}
                :two {:be {956 {:summary (atom "hello world")}}}}
               (:graph result)))
        (is (= 1 @calls))))
    (testing "should do precedence stripping."
      (let [rating (clojure.core/atom 0)
            title (clojure.core/atom 0)
            router (precedence-router
                     (fn [path]
                       (swap! title inc)
                       (is (= [:videos [123] :title]
                              path)))
                     (fn [path]
                       (swap! rating inc)
                       (is (= [:videos [123] :rating]
                              path))))
            results (gets router [[:videos 123 [:title :rating]]] {})
            result (first results)]
        (is (= 1 (count results)))
        (is (= {:videos {123 {:title (atom "title 123")
                              :rating (atom "rating 123")}}}
               (:graph result)))
        (is (= 1 @title))
        (is (= 1 @rating))))
    (testing "should do precedence matching."
      (let [specific (clojure.core/atom 0)
            keys (clojure.core/atom 0)
            router (router [{:route [:a :specific]
                             :get (fn [_ _]
                                    (swap! specific inc)
                                    [(path-value [:a :specific] "hello world")])}
                            {:route [:a KEYS]
                             :get (fn [_ _]
                                    (swap! keys inc)
                                    [(path-value [:a :specific] "hello world")])}])
            _ (get router [[:a :specific]])]
        (is (= 1 @specific))
        (is (= 0 @keys))))
    (testing "should grab a reference."
      (let [router (precedence-router nil nil)
            results (gets router [[:lists :abc 0]] {})]
        (is (= 1 (count results)))
        (is (= {:lists {:abc {0 (ref [:videos 0])}}}
               (:graph (last results))))))
    (testing "should not follow references if no keys specified after path to reference"
      (let [router (router [{:route [:products-by-id KEYS KEYS]
                             :get (fn [_ _] (throw (ex-info "reference followed in error" {})))}
                            {:route [:proffers-by-id INTEGERS :products-list RANGES]
                             :get (fn [_ _] [(path-value [:proffers-by-id 1 :products-list 0]
                                                         (ref [:products-by-id "CSC1471105X"]))
                                             (path-value [:proffers-by-id 1 :products-list 1]
                                                         (ref [:products-by-id "HON4033T"]))])}])]
        (is (= {:proffers-by-id {1 {:products-list {0 (ref [:products-by-id "CSC1471105X"])
                                                    1 (ref [:products-by-id "HON4033T"])}}}}
               (:graph (get router [[:proffers-by-id 1 :products-list (range 0 2)]]))))))))

;; TODO: copy the falcor set tests ... not 100% sold on impl working on harder examples!
(deftest router-set-test
  (let [users-router (fn []
                       (let [users (clojure.core/atom {1 "Erik"
                                                       2 "Jack"})
                             router (router [{:route [:users RANGES]
                                              :get (fn [[_ ranges] _]
                                                     (let [users (vec @users)]
                                                       (for [idx (indices ranges)
                                                             :let [[id _] (clojure.core/get users idx)]]
                                                         (core/path-value [:users idx]
                                                                          (if id
                                                                            (ref [:user/by-id id])
                                                                            (atom))))))}
                                             {:route [:user/by-id INTEGERS :user/name]
                                              :set (fn [pathmap _]
                                                     (doall (for [[id {:keys [user/name]}] (clojure.core/get pathmap :user/by-id)]
                                                              (do (swap! users assoc id name)
                                                                  (core/path-value [:user/by-id id :user/name]
                                                                                   name)))))}])]
                         [users router]))]
    (testing "can set path without a ref"
      (let [[users router] (users-router)
            result (set router [{:user/by-id {1 {:user/name "Huey"}}}])]
        (is (= "Huey"
               (clojure.core/get @users 1)))
        (is (= {:user/by-id {1 {:user/name (atom "Huey")}}}
               (:graph result)))))
    (testing "can set path with a ref"
      (let [[users router] (users-router)
            result (set router [{:users {0 {:user/name "Huey"
                                            :user/age 13}}}])]
        (is (= "Huey"
               (clojure.core/get @users 1)))
        (is (= {:users {0 (ref [:user/by-id 1])}
                :user/by-id {1 {:user/name (atom "Huey")}}}
               (:graph result)))))))

(deftest router-call-test
  (let [users-router (fn []
                       (let [users (clojure.core/atom {1 {:user/name "Erik"}
                                                       2 {:user/name "Jack"}})
                             router (router [{:route [:users RANGES]
                                              :get (fn [[_ ranges] _]
                                                     (for [idx (indices ranges)]
                                                       (core/path-value [:users idx]
                                                                        (ref [:user/by-id
                                                                              (clojure.core/get (into [] (keys @users)) idx)]))))}
                                             {:route [:users :add]
                                              :call (fn [_ {:keys [user/name]} _]
                                                      (let [user-id 7
                                                            count (count @users)]
                                                        (swap! users assoc user-id {:user/name name})
                                                        [{:users {count (ref [:user/by-id user-id])}}]))}
                                             {:route [:users :length]
                                              :get (fn [_ _]
                                                     [(core/path-value [:users :length]
                                                                       (count @users))])}
                                             {:route [:user/by-id INTEGERS :user/name]
                                              :get (fn [[_ ids] _]
                                                     (for [id ids]
                                                       (core/path-value [:user/by-id id :user/name]
                                                                        (get-in @users [id :user/name]))))}
                                             {:route [:user/by-id INTEGERS :user/friend]
                                              :get (fn [[_ ids] _]
                                                     (for [id ids
                                                           :let [friend-id (get-in @users [id :user/friend])]
                                                           :when friend-id]
                                                       (core/path-value [:user/by-id id :user/friend]
                                                                        (ref [:user/by-id friend-id]))))}
                                             {:route [:user/by-id INTEGERS :user/set-name]
                                              :call (fn [[_ [id]] {:keys [user/name]} _]
                                                      (swap! users assoc-in [id :user/name] name)
                                                      [])}
                                             {:route [:user/by-id INTEGERS :user/set-friend]
                                              :call (fn [[_ [id]] args _]
                                                      (let [friend-id (:user/id args)]
                                                        (swap! users assoc-in [id :user/friend] friend-id)
                                                        [(core/path-value [:user/by-id id :user/friend]
                                                                          (ref [:user/by-id friend-id]))]))}]
                                            {:call (fn [runner]
                                                     (fn [match context]
                                                       (runner match context)))})]
                         [users router]))]
    (testing "can call a mutation"
      (let [[users router] (users-router)
            result (call router [:users :add] {:user/name "Mike"} {:refs [[:user/name]]
                                                                   :this [[:length]]})]
        (is (= #{"Erik" "Jack" "Mike"}
               (into #{} (map :user/name (vals @users)))))
        (is (= {:users {2 (ref [:user/by-id 7])
                        :length (atom 3)}
                :user/by-id {7 {:user/name (atom "Mike")}}}
               (:graph result)))))
    (testing "can call a mutation with optimization"
      (let [[users router] (users-router)
            result (call router [:users 0 :user/set-name] {:user/name "Huey"} {:this [[:user/name]]})]
        (is (= #{"Huey" "Jack"}
               (into #{} (map :user/name (vals @users)))))
        (is (= {:users {0 (ref [:user/by-id 1])}
                :user/by-id {1 {:user/name (atom "Huey")}}}
               (:graph result)))))
    (testing "can call a mutation with optimization and ref"
      (let [[users router] (users-router)
            result (call router [:users 0 :user/set-friend] {:user/id 2} {:refs [[:user/name]]})]
        (is (= 2
               (get-in @users [1 :user/friend])))
        (is (= {:users {0 (ref [:user/by-id 1])}
                :user/by-id {1 {:user/friend (ref [:user/by-id 2])}
                             2 {:user/name (atom "Jack")}}}
               (:graph result)))))))
