(ns cambo.router
  (:refer-clojure :exclude [get set])
  (:require [cambo.core :as core]
            [cambo.graph :as graph]
            [cambo.core :as cam]))

;;; ROUTES

(defprotocol IRouteKey
  (strip [this keyset]))

(defn strip-collect
  [route-key coll]
  (reduce (fn [[int diff] keyset]
            (let [[int' diff'] (strip route-key keyset)]
              [(into int int')
               (into diff diff')]))
          [[] []]
          coll))

(def KEYS
  (reify
    IRouteKey
    (strip [this keyset]
      (if (vector? keyset)
        [keyset []]
        [[keyset] []]))))

(def INTEGERS
  (reify
    IRouteKey
    (strip [this keyset]
      (cond
        (integer? keyset) [[keyset] []]
        (core/range? keyset) [[keyset] []]
        (core/key? keyset) [[] [keyset]]
        (vector? keyset) (strip-collect this keyset)
        :else (strip-collect this (core/keys keyset))))))

(def RANGES
  (reify
    IRouteKey
    (strip [this keyset]
      (cond
        (integer? keyset) [[keyset] []]
        (core/range? keyset) [[keyset] []]
        (core/key? keyset) [[] [keyset]]
        (vector? keyset) (strip-collect this keyset)
        :else (strip-collect this (core/keys keyset))))))

(defn virtual-key? [x]
  (or (= RANGES x)
      (= INTEGERS x)
      (= KEYS x)))

(defn route-key? [x]
  (or (virtual-key? x)
      (core/key? x)))

(defn route-keyset? [x]
  (or (route-key? x)
      (and (vector? x)
           ;; virtual keys not allowed in keysets -- only top level
           ;; ranges not allowed at all
           (every? core/key? x))))

(defn route-keys [x]
  (cond
    (or (virtual-key? x)
        (core/key? x)) [x]
    (vector? x) x))

(defn route? [x]
  (and (vector? x)
       (every? route-key? x)))

(defn routeset? [x]
  (and (vector? x)
       (every? route-keyset? x)))

(defn route-hash [route]
  (into [] (for [key route]
             (condp = key
               RANGES ::i
               INTEGERS ::i
               KEYS ::k
               key))))

(defn expand-routeset [[route-keyset & routeset]]
  (if (seq routeset)
    (for [key (route-keys route-keyset) p (expand-routeset routeset)]
      (into [key] p))
    (map vector (route-keys route-keyset))))

(defn assert-route-hashes
  [handlers]
  (doall (->> handlers
              (mapcat (fn [[{:keys [get set call]} route]]
                        (let [hash (route-hash route)]
                          (cond-> []
                                  get (conj (conj hash ::get))
                                  set (conj (conj hash ::set))
                                  call (conj (conj hash ::call))))))
              (reduce (fn [hashes hash]
                        (when (contains? hashes hash)
                          (throw (ex-info "route conflict" {:hash hash})))
                        (conj hashes hash))
                      #{}))))

(defonce route-ids (atom 0))
(def id-key ::id)
(def match-key ::match)

;; rename handler - route-record / route-object ... handler no beuno
(defn route-tree
  [handlers]
  (let [handlers (concat (for [{:keys [route] :as handler} handlers
                               ;; falcor does id per call / set / get -- but don't think it matters?
                               :let [handler (assoc handler id-key (swap! route-ids inc))]
                               route (expand-routeset route)]
                           [handler route]))]
    (assert-route-hashes handlers)
    (loop [tree {} handlers handlers]
      (if-let [[{:keys [get set call] :as handler} route] (first handlers)]
        (recur (cond-> tree
                       get (assoc-in (into route [match-key :get]) handler)
                       set (assoc-in (into route [match-key :set]) handler)
                       call (assoc-in (into route [match-key :call]) handler))
               (rest handlers))
        tree))))

(defn branches [keyset tree]
  (let [keys (into [] (core/keys keyset))
        ints (into [] (filter integer? keys))
        route-keys (cond-> (conj keys KEYS)
                           (seq ints) (conj INTEGERS RANGES))
        branch-keys (select-keys tree route-keys)]
    (map (fn [[route-key sub-tree]]
           (let [matched-keys (condp = route-key
                                INTEGERS ints
                                RANGES ints
                                KEYS keys
                                route-key)]
             [matched-keys route-key sub-tree]))
         branch-keys)))

(defn match* [tree method pathset request virtual]
  ;; set / call only match full paths, get does the path optimization
  (let [method' (if (seq pathset)
                       :get
                       method)
        match (when-let [handler (get-in tree [::match method'])]
                {:method method'
                 :request request
                 :virtual virtual
                 :suffix (vec pathset)
                 :handler handler})]

    (cond-> []
            match
            (conj match)

            (seq pathset)
            (into (mapcat (fn [[route-key virtual-key sub-tree]]
                            (match* sub-tree
                                    method
                                    (rest pathset)
                                    (conj request route-key)
                                    (conj virtual virtual-key)))
                          (branches (first pathset) tree))))))

(defn collapse-matches
  [matches]
  (let [{:keys [method handler virtual suffix]} (first matches)
        pathsets (core/collapse (map :request matches))]
    (into [] (for [pathset pathsets]
               {:method method
                :handler handler
                :request pathset
                :suffix suffix
                :virtual (mapv (fn [vkey key]
                                 (condp = vkey
                                   RANGES vkey
                                   KEYS vkey
                                   INTEGERS vkey
                                   key))
                               virtual pathset)}))))

(defn match [tree method pathset]
  (let [matches (match* tree method pathset [] [])
        reduced (group-by (comp id-key :handler) matches)
        collapsed (into [] (mapcat (comp collapse-matches second)
                                   reduced))]
    collapsed))

(defn strip-primitive
  [prim keyset]
  (cond
    (= prim keyset) [[keyset] []]
    (vector? keyset) (strip-collect prim keyset)
    :else [[] [keyset]]))

(extend-protocol IRouteKey
  #?(:clj clojure.lang.Keyword
     :cljs cljs.core.Keyword)
  (strip [this keyset]
    (strip-primitive this keyset))
  #?(:clj String
     :cljs string)
  (strip [this keyset]
    (strip-primitive this keyset))
  #?(:clj clojure.lang.Symbol
     :cljs cljs.core.Symbol)
  (strip [this keyset]
    (strip-primitive this keyset))
  #?(:clj Number
     :cljs number)
  (strip [this keyset]
    (cond
      (= this keyset) [[keyset] []]
      (core/range? keyset) (let [{:keys [start end]} keyset]
                             (cond
                               (= this start) [[this] [(core/range (inc start) end)]]
                               (< start this (dec end)) [[this] [(core/range start this)
                                                                 (core/range (inc this) end)]]
                               (= this (dec end)) [[this] [(core/range start this)]]
                               :else [[] [keyset]]))
      (vector? keyset) (strip-collect this keyset)
      :else [[] [keyset]]))
  #?(:clj clojure.lang.APersistentVector
     :cljs cljs.core.PersistentVector)
  (strip [this keyset]
    (reduce (fn [[int diff] route-key]
              (let [[int' diff'] (strip route-key diff)]
                [(into int int')
                 diff']))
            [[] (if (vector? keyset)
                  keyset
                  [keyset])]
            this)))

(defn strip-path
  "assumes there is an intersection"
  [[routekey & routeset] [keyset & pathset :as all]]
  (let [[key-int key-diffs] (strip routekey keyset)
        key-int (if (= 1 (count key-int))
                  (first key-int)
                  key-int)
        diffs (mapv (fn [key-diff]
                     (into [key-diff] pathset))
                   key-diffs)]
    (if (seq routeset)
      (let [[path-int path-diffs] (strip-path routeset pathset)]
        [(into [key-int] path-int)
         (into diffs
               (map (fn [diff] (into [key-int] diff)) path-diffs))])
      [[key-int] diffs])))

(defn intersects?
  [[routekey & route] [keyset & pathset]]
  (let [[int _] (strip routekey keyset)]
    (cond
      (empty? int) false
      (seq route) (intersects? route pathset)
      :else true)))

(defn key-precedence [key]
  (condp = key
    KEYS 1
    INTEGERS 2
    RANGES 2
    4))

(defn ranges [ns]
  (->> ns
       distinct
       sort
       (reduce (fn [acc n]
                 (let [a (-> acc last last)]
                   (if (or (nil? a)
                           (not= n (inc a)))
                     (conj acc [n])
                     (update acc (dec (count acc)) conj n))))
               [])
       (map (fn [ns]
              (let [from (first ns)
                    to (last ns)]
                (core/range from (inc to)))))
       (into [])))

(defn conform-path [routeset pathset]
  (letfn [(conform-key [route-key keyset]
            (cond
              (= route-key KEYS) (into [] (core/keys keyset))
              (= route-key INTEGERS) (into [] (core/keys keyset))
              (= route-key RANGES) (ranges (core/keys keyset))
              (vector? route-key) (if (vector? keyset)
                                    keyset
                                    [keyset])
              :else (first (core/keys keyset))))]
    (into [] (map conform-key routeset pathset))))

(defn precedence [route]
  (mapv key-precedence route))

(def desc #(compare %2 %1))

(defn executable-matches
  [matches pathset]
  (letfn [(collect-matches [{:keys [virtual handler] :as match} pathsets]
            (reduce (fn [[results pathsets] pathset]
                      (if (intersects? virtual pathset)
                        (let [[intersection differences] (strip-path virtual pathset)
                              route (:route handler)]
                          [(conj results (assoc match :pathset (conform-path route intersection)))
                           (into pathsets differences)])
                        [results (conj pathsets pathset)]))
                    [[] []]
                    pathsets))]
    (loop [matches (sort-by (comp precedence :virtual) desc matches)
           remaining-pathsets [pathset]
           results []]
      (if (and (seq remaining-pathsets)
               (seq matches))
        (let [match (first matches)
              [match-results remaining-pathsets] (collect-matches match remaining-pathsets)]
          (recur (rest matches)
                 (core/collapse remaining-pathsets)
                 (into results match-results)))
        {:matches results
         :unhandled remaining-pathsets}))))

(defn get-executable-matches
  [route-tree method pathsets]
  (reduce (fn [results pathset]
            (let [matches (match route-tree method pathset)
                  {:keys [matches unhandled]} (executable-matches matches pathset)]
              (-> results
                  (update :matches into matches)
                  (update :unhandled into unhandled))))
          {:matches []
           :unhandled []}
          pathsets))

(defn indices [ranges]
  (mapcat core/keys ranges))

(defprotocol IRouteResult
  (update-context [this context match]))

(extend-protocol IRouteResult
  cambo.core.PathValue
  (update-context [{:keys [path value] :as pv} context {:keys [suffix]}]
    (let [context (-> context
                      (update :graph graph/set-path-value pv)
                      (update :optimized conj path))]
      (cond-> context
              (and (core/ref? value) (seq suffix))
              (update :pathsets conj (into (:path value) suffix)))))

  #?(:clj clojure.lang.APersistentMap
     :cljs cljs.core.PersistentArrayMap)
  (update-context [pathmap context match]
    (reduce (fn [context pv]
              (update-context pv context match))
            context
            (core/pathmap-values pathmap)))

  #?(:cljs cljs.core.PersistentHashMap)
  #?(:cljs (update-context [pathmap context match]
                           (reduce (fn [context pv]
                                     (update-context pv context match))
                                   context
                                   (core/pathmap-values pathmap)))))

(deftype Invalidate [path]
  IRouteResult
  (update-context [_ context _]
    (update context :invalidate conj path)))

(defn invalidate [path]
  (Invalidate. path))

(deftype AdditionalPaths [pathsets]
  IRouteResult
  (update-context [_ context _]
    (update context :pathsets into pathsets)))

(defn additional-paths [paths]
  (AdditionalPaths. paths))

(deftype SetMethod [method]
  IRouteResult
  (update-context [_ context _]
    (assoc context :method method)))

(defn set-method [method]
  (SetMethod. method))

(defn merge-results
  [context results]
  (letfn [(optimize-pathsets [{:keys [pathsets graph] :as context}]
            (assoc context :pathsets (->> pathsets
                                          (core/optimize graph)
                                          core/collapse
                                          (into []))))]
    (let [context (reduce (fn [context [match value]]
                            (update-context value context match))
                          context
                          results)
          context (optimize-pathsets context)]
      context)))

(defn init-context
  [method pathsets env]
  {:method method
   :pathsets pathsets
   :env env
   :graph {}
   ;; might want to just remove this -- want to support streaming requests at some point
   :request (vec pathsets)
   :matched []
   :optimized []
   :unhandled []
   :invalidate []})

(defn execute
  [route-tree context runner]
  (letfn [(context-result [context]
            (dissoc context :method :pathsets :env))
          (execute* [{:keys [pathsets method] :as context}]
            (lazy-seq
              (if (seq pathsets)
                (let [context (assoc context :pathsets [])
                      {:keys [matches unhandled]} (get-executable-matches route-tree method pathsets)
                      context (update context :unhandled into unhandled)
                      results (for [match matches
                                    value (runner match context)]
                                [match value])
                      context (merge-results context results)]
                  (cons (context-result context)
                        (execute* context)))
                nil)))]
    (execute* context)))

(defn gets [{:keys [route-tree get-middleware]} pathsets env]
  (letfn [(runner [{:keys [handler pathset]} {:keys [env]}]
            ((:get handler) pathset env))]
    (execute route-tree
             (init-context :get pathsets env)
             (cond-> runner get-middleware get-middleware))))

(defn get
  ([router pathsets]
   (get router pathsets {}))
  ([router pathsets env]
   (last (gets router pathsets env))))

(defn pull
  ([router query]
   (pull router query {}))
  ([router query env]
   (let [pathsets (core/query-pathsets query)
         result (last (gets router pathsets env))]
     (into {} (for [[key value] result]
                (if (not= key :graph)
                  [key (core/pathsets-query value)]
                  [key value]))))))

(defn sets [{:keys [route-tree set-middleware]} pathmaps env]
  (let [{:keys [graph paths]} (graph/set {} pathmaps)
        ;; TODO: not sure if this is necessary
        paths (core/expand-pathsets paths)]
    (letfn [(runner [{:keys [method handler pathset request virtual]} {:keys [env] :as context}]
              (case method
                :get ((:get handler) pathset env)
                :set (let [cache (:graph context)
                           optimized-with-path (for [path paths
                                                     :let [optimized (first (core/optimize cache [path]))]
                                                     :when (intersects? virtual optimized)]
                                                 [optimized path])
                           set-graph (reduce (fn [set-graph [optimized path]]
                                               (graph/set-path-value set-graph
                                                                     optimized
                                                                     (graph/get-value graph path)))
                                             {}
                                             optimized-with-path)
                           pathmap (:graph (graph/get set-graph [request]))]
                  ((:set handler) pathmap env))))]
      (let [pathsets (core/collapse paths)]
        (execute route-tree
                 (init-context :set pathsets env)
                 (cond-> runner set-middleware set-middleware))))))

(defn set
  ([router pathmaps]
   (set router pathmaps {}))
  ([router pathmaps env]
   (last (sets router pathmaps env))))

(defn calls [{:keys [route-tree call-middleware]} call-path args queries env]
  (letfn [(runner [{:keys [method handler request pathset]} {:keys [env]}]
            (case method
              :get ((:get handler) pathset env)
              :call (let [results ((:call handler) pathset args env)
                          ;; expand pathmaps so we can search for refs
                          results (into [] (mapcat (fn [result]
                                                     (if (core/pathmap? result)
                                                       (core/pathmap-values result)
                                                       [result]))
                                                   results))
                          refs (filter (fn [result]
                                         (and (core/path-value? result)
                                              (core/ref? (:value result))))
                                       results)
                          results (conj results (set-method :get))
                          ;; I don't think this distinction matters for our impl of falcor
                          deopt-path (into [] (butlast call-path))
                          this-path (into [] (butlast pathset))
                          this-paths (into []
                                           (for [suffix (:this queries)]
                                             (into this-path suffix)))
                          ref-paths  (into []
                                           (for [ref refs
                                                 :let [base-path (into deopt-path
                                                                       (drop (dec (count request)) (:path ref)))]
                                                 suffix (:refs queries)]
                                             (into base-path suffix)))
                          results (cond-> results
                                          (seq this-paths) (conj (additional-paths this-paths))
                                          (seq ref-paths) (conj (additional-paths ref-paths)))]
                      results)))]
    (execute route-tree
             (init-context :call [call-path] env)
             (cond-> runner call-middleware call-middleware))))

(defn call
  ([router path args queries]
   (call router path args queries {}))
  ([router path args queries env]
   (let [queries (into {} (for [[name query] queries]
                            [name (cam/query-pathsets query)]))]
     (last (calls router path args queries env)))))

;; TODO: this name sucks
(defn handle
  ([router request]
   (handle router request {}))
  ([router {:keys [method] :as request} env]
   (case method
     :get (let [{:keys [pathsets]} request]
            (get router pathsets env))
     :pull (let [{:keys [query]} request]
            (pull router query env))
     :set (let [{:keys [pathmaps]} request]
            (set router pathmaps env))
     :call (let [{:keys [path args queries]} request]
             (call router path args queries env)))))

(defrecord Router [route-tree])

(defn router
  ([routes]
   (Router. (route-tree routes)))
  ([routes {:keys [get set call]}]
   (let [config (cond-> {:route-tree (route-tree routes)}
                        get (assoc :get-middleware get)
                        set (assoc :set-middleware set)
                        call (assoc :call-middleware call))]
     (map->Router config))))

(defrecord RouterDatasource
  [router env]
  core/IDataSource
  (pull [_ query cb]
    ;; TODO: doesn't impl `:missing`
    (cb (pull router query env)))
  (set [_ pathmaps cb]
    (cb (set router pathmaps env)))
  (call [_ path args queries cb]
    (cb (call router path args queries env))))

(defn as-datasource
  ([router]
   (as-datasource router {}))
  ([router env]
   (RouterDatasource. router env)))
