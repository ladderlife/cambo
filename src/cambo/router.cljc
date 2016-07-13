(ns cambo.router
  (:refer-clojure :exclude [get set])
  (:require [cambo.core :as core]
            [cambo.graph :as graph]))

;;; ROUTES

;; TODO: is this a legit way to make a unique value? (don't want keyword -- clash with `keys`)
(def RANGES (symbol "RANGES"))
(def INTEGERS (symbol "INTEGERS"))
(def KEYS (symbol "KEYS"))

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

(defn match-virtual? [virtual key]
  (condp = virtual
    KEYS true
    INTEGERS (integer? key)
    RANGES (integer? key)
    (= key virtual)))

(defn difference [keyset virtual]
  (->> (core/keys keyset)
       (remove (fn [key]
                 (some #(match-virtual? % key) (route-keys virtual))))))

(defn intersection [keyset virtual]
  (->> (core/keys keyset)
       (filter (fn [key]
                 (some #(match-virtual? % key) (route-keys virtual))))))

(defn path-intersection [[ks & path] [virtual & virtual-path]]
  (let [int (core/keyset (intersection ks virtual))]
    (when int
      (cond-> [int]
              (seq path) (into (path-intersection path virtual-path))))))

(defn path-difference [[ks & path] [virtual & virtual-path]]
  (let [diff (core/keyset (difference ks virtual))
        int (core/keyset (intersection ks virtual))]
    (cond-> []
            (seq (core/keys diff)) (conj (into [diff] path))
            (and int (seq virtual-path)) (into (map (partial into [int])
                                                    (path-difference path virtual-path))))))

(defn intersects?
  [[ks & path] [virtual & virtual-path]]
  (let [int (intersection ks virtual)]
    (cond
      (empty? int) false
      (seq path) (intersects? path virtual-path)
      :else true)))

;; TODO: steal the falcor strip impl -- this was dumb idea & slow
(defn strip-path [pathset routeset]
  [(path-intersection pathset routeset)
   (path-difference pathset routeset)])

(defn key-precedence [key]
  (condp = key
    KEYS 1
    INTEGERS 2
    RANGES 2
    4))

(defn precedence [route]
  (mapv key-precedence route))

(def desc #(compare %2 %1))

(defn executable-matches
  [matches pathset]
  (loop [results []
         matches (sort-by (comp precedence :virtual) desc matches)
         pathsets [pathset]]
    (let [{:keys [virtual] :as match} (first matches)]
      (if (and match (seq pathsets))
        (let [[path-matches remaining-pathsets] (loop [pathsets pathsets path-matches [] remaining-pathsets []]
                                                  (if-let [pathset (first pathsets)]
                                                    (let [[intersection differences] (strip-path pathset virtual)]
                                                      (if intersection
                                                        (recur (rest pathsets)
                                                               (conj path-matches {:path intersection
                                                                                   :match match})
                                                               (into remaining-pathsets differences))
                                                        (recur (rest pathsets) path-matches remaining-pathsets)))
                                                    [path-matches remaining-pathsets]))]
          (recur (into results path-matches)
                 (rest matches)
                 remaining-pathsets))
        ;; TODO: the value of `pathsets` here are the `unhandled pathsets` -- we want this infos eventually
        results))))

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

(defn indices [ranges]
  (mapcat core/keys ranges))

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

(defn run
  [context pathset matches runner]
  (let [path-matches (executable-matches matches pathset)]
    (for [{:keys [path match]} path-matches
          :let [{:keys [handler]} match
                {:keys [route]} handler
                path (conform-path route path)]]
      [match (runner match path context)])))

(defn merge-path-value
  [context {:keys [suffix]} {:keys [value path] :as pv}]
  (let [context (-> context
                    (update :graph graph/set-path-value pv)
                    ;; TODO: we need both paths (optimized / requested)
                    (update :paths conj path))]
    [context (if (and (core/ref? value) (seq suffix))
               [(into (:path value) suffix)]
               [])]))

(defn merge-pathmap
  [context match pathmap]
  (let [pvs (core/pathmap-values pathmap)]
    (reduce (fn [[context paths] pv]
              (let [[context add-paths] (merge-path-value context match pv)]
                [context (into paths add-paths)]))
            [context []]
            pvs)))

(defn merge-invalidate
  [context _ {:keys [path]}]
  [(update context :invalidate (fnil conj []) path) []])

(defn merge-additional-paths
  [context _ {:keys [paths]}]
  [context paths])

(defn merge-set-method
  [context _ {:keys [method]}]
  [(assoc context :method method) []])

(defn merge-result
  [context match value]
  (cond
    (core/path-value? value) (merge-path-value context match value)
    (core/additional-paths? value) (merge-additional-paths context match value)
    (core/set-method? value) (merge-set-method context match value)
    (core/invalidate? value) (merge-invalidate context match value)
    (core/pathmap? value) (merge-pathmap context match value)))

(defn merge-results
  [context results]
  (loop [context context paths [] results results]
    (if-let [[match value] (first results)]
      (let [[context new-paths] (merge-result context match value)]
        (recur context (into paths new-paths) (rest results)))
      [context (->> paths
                    (core/optimize (:graph context))
                    core/collapse)])))

(defn execute
  [route-tree method runner pathsets]
  (letfn [(match-and-run [{:keys [method] :as context} pathset]
            (let [matches (match route-tree method pathset)
                  results (run context pathset matches runner)]
              results))
          (execute* [context pathsets]
            (lazy-seq
              (if (seq pathsets)
                (let [results (mapcat (partial match-and-run context) pathsets)
                      ;; expanding values -> value will be impl dependant
                      ;; here it is just a seq
                      results (for [[match values] results
                                    value values]
                                [match value])
                      [context paths] (merge-results context results)]
                  (cons (dissoc context :method)
                        (execute* context paths)))
                nil)))]
    (execute* {:method method
               :graph {}
               :paths []}
              pathsets)))

(defn gets [{:keys [route-tree]} pathsets ctx]
  (letfn [(runner [{:keys [handler]} path _]
            ((:get handler) path ctx))]
    (execute route-tree :get runner pathsets)))

(defn get
  ([router pathsets]
   (get router pathsets {}))
  ([router pathsets ctx]
   (last (gets router pathsets ctx))))

(defn sets [{:keys [route-tree]} pathmaps ctx]
  (let [{:keys [graph paths]} (graph/set {} pathmaps)
        ;; TODO: not sure if this is necessary
        paths (core/expand-pathsets paths)]
    (letfn [(runner [{:keys [method handler request virtual]} pathset execution-context]
              (case method
                :get ((:get handler) pathset ctx)
                :set (let [cache (:graph execution-context)
                           optimized-with-path (for [path paths
                                                     :let [optimized (first (core/optimize cache [path]))]
                                                     :when (intersects? optimized virtual)]
                                                 [optimized path])
                           set-graph (reduce (fn [set-graph [optimized path]]
                                               (graph/set-path-value set-graph
                                                                     optimized
                                                                     (graph/get-value graph path)))
                                             {}
                                             optimized-with-path)
                           pathmap (:graph (graph/get set-graph [request]))]
                  ((:set handler) pathmap ctx))))]
      (let [pathsets (core/collapse paths)]
        (execute route-tree :set runner pathsets)))))

(defn set
  ([router pathmaps]
   (set router pathmaps {}))
  ([router pathmaps ctx]
   (last (sets router pathmaps ctx))))

(defn calls [{:keys [route-tree]} call-path args queries ctx]
  (letfn [(runner [{:keys [method handler request]} matched-path _]
            (case method
              :get ((:get handler) matched-path ctx)
              :call (let [results ((:call handler) matched-path args ctx)
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
                          results (conj results (core/set-method :get))
                          ;; I don't think this distinction matters for our impl of falcor
                          deopt-path (into [] (butlast call-path))
                          this-path (into [] (butlast matched-path))
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
                                          (seq this-paths) (conj (core/additional-paths this-paths))
                                          (seq ref-paths) (conj (core/additional-paths ref-paths)))]
                      results)))]
    (execute route-tree :call runner [call-path])))

(defn call
  ([router path args queries]
   (call router path args queries {}))
  ([router path args queries ctx]
   (last (calls router path args queries ctx))))

(defrecord Router [route-tree])

(defn router
  [routes]
  (Router. (route-tree routes)))

(defrecord RouterDatasource
  [router ctx]
  core/IDataSource
  (get [_ pathsets cb]
    ;; TODO: doesn't impl `:missing`
    (cb (get router pathsets ctx)))
  (set [_ pathmaps cb]
    (cb (set router pathmaps ctx)))
  (call [_ path args queries cb]
    (cb (call router path args queries ctx))))

(defn as-datasource
  ([router]
   (as-datasource router {}))
  ([router ctx]
   (RouterDatasource. router ctx)))
