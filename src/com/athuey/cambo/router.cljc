(ns com.athuey.cambo.router
  (:refer-clojure :exclude [get set])
  (:require [com.athuey.cambo.core :as core]
            [com.athuey.cambo.graph :as graph]))

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
  [routes]
  (loop [hashes #{} [route & routes] routes]
    (when route
      (let [hash (route-hash route)]
        (if (contains? hashes hash)
          (throw (ex-info "route conflict" {:route route}))
          (recur (conj hashes hash)
                 (rest routes)))))))


(defonce route-ids (atom 0))
(def id-key ::id)
(def match-key ::match)

(defn route-tree
  [handlers]
  (let [handlers (concat (for [{:keys [route] :as handler} handlers
                               :let [handler (assoc handler id-key (swap! route-ids inc))]
                               route (expand-routeset route)]
                           [handler route]))]
    (assert-route-hashes (map second handlers))
    (loop [tree {} handlers handlers]
      (if-let [[handler route] (first handlers)]
        (recur (assoc-in tree (conj route match-key) handler)
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

(defn match* [tree pathset request virtual pred]
  (cond-> []
          (and (contains? tree ::match)
               (pred (::match tree)))
          (conj {:request request
                 :virtual virtual
                 :suffix (vec pathset)
                 :handler (::match tree)})

          (seq pathset)
          (into (mapcat (fn [[route-key virtual-key sub-tree]]
                          (match* sub-tree
                                  (rest pathset)
                                  (conj request route-key)
                                  (conj virtual virtual-key)
                                  pred))
                        (branches (first pathset) tree)))))

(defn collapse-matches
  [matches]
  (let [{:keys [handler virtual suffix]} (first matches)
        pathsets (core/collapse (map :request matches))]
    (into [] (for [pathset pathsets]
               {:handler handler
                :request pathset
                :suffix suffix
                :virtual (mapv (fn [vkey key]
                                 (condp = vkey
                                   RANGES vkey
                                   KEYS vkey
                                   INTEGERS vkey
                                   key))
                               virtual pathset)}))))

(defn match [tree pathset pred]
  (let [matches (match* tree pathset [] [] pred)
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
  [pathset matches runner]
  (let [path-matches (executable-matches matches pathset)]
    (for [{:keys [path match]} path-matches
          :let [{:keys [handler]} match
                {:keys [route]} handler
                path (conform-path route path)]
          result (runner handler path)]
      [match result])))

;; TODO: handle more than pv
;; TODO: don't optimize stuff here -- do it only merge-results

(defn merge-result
  [cache result]
  (let [[{:keys [suffix]} {:keys [value] :as pv}] result]
    (let [cache (graph/set-path-value cache pv)
          paths (when (and (core/ref? value) (seq suffix))
                  (->> [(into (:path value) suffix)]
                       (core/optimize cache)
                       core/collapse))]
      [cache paths])))

(defn merge-results
  [cache results]
  (loop [cache cache paths [] results results]
    (if-let [result (first results)]
      (let [[cache new-paths] (merge-result cache result)]
        (recur cache (into paths new-paths) (rest results)))
      [cache (->> paths
                  (core/optimize cache)
                  core/collapse)])))

(defn execute
  [router matcher runner pathsets]
  (letfn [(match-and-run [pathset]
            (let [matches (matcher router pathset)
                  results (run pathset matches runner)]
              results))
          (execute* [cache pathsets]
            (lazy-seq
              (if (seq pathsets)
                (let [[cache paths] (->> pathsets
                                         (mapcat match-and-run)
                                         (merge-results cache))]
                  (cons cache (execute* cache paths)))
                nil)))]
    (execute* {} pathsets)))

(defn gets [router pathsets]
  (letfn [(matcher [tree pathset]
            (match tree pathset :get))
          (runner [{:keys [get]} path]
            (get path))]
    (execute router matcher runner pathsets)))

(defrecord Router [route-tree])

(extend-type Router
  core/IDataSource
  (get [{:keys [route-tree]} pathsets]
    ;; TODO: doesn't impl `:missing`
    {:graph (last (gets route-tree pathsets))})
  (set [_ _]
    (throw (ex-info "no impl" {}))))

(defn router
  [routes]
  (Router. (route-tree routes)))
