(ns cambo.core
  (:refer-clojure :exclude [get set atom ref keys range ->Atom Atom ->Ref Ref ->Range Range]))

;;; DATASOURCE

(defprotocol IDataSource
  ;; TODO: do we want this to be a single call cb, or observable?
  (pull [this query cb])
  (set [this pathmaps cb])
  (call [this path args queries cb]))

;;; GRAPH

(defrecord Atom [])

(defn atom
  ([]
   (Atom.))
  ([value]
   (map->Atom {:value value})))

(defn atom? [x]
  (instance? Atom x))

(defn non-value? [x]
  (and (atom? x)
       (not (contains? x :value))))

(defrecord Ref [path])

(defn ref [path]
  (Ref. path))

(defn ref? [x]
  (instance? Ref x))

(defn boxed?
  [value]
  (or (atom? value)
      (ref? value)))

(defrecord PathValue [path value])

(defn pv [path value]
  (PathValue. path value))

(defn path-value [path value]
  (PathValue. path value))

(defn path-value? [x]
  (instance? PathValue x))

(defn pathmap [path value]
  (assoc-in {} path value))

(defn pathmap? [x]
  (and (map? x)
       (not (record? x))))

;;; KEYS

(defn uuid? [x]
  #?(:clj (instance? java.util.UUID x)
     :cljs (or (instance? cljs.core/UUID x)
               (instance? com.cognitect.transit.types.UUID x))))

(defn key? [x]
  (or (string? x)
      (keyword? x)
      (integer? x)
      (symbol? x)
      (uuid? x)))

(defrecord Range [start end])

(defn range [start end]
  (Range. start end))

(defn range-keys [{:keys [start end]}]
  (clojure.core/range start end))

(defn range? [x]
  (instance? Range x))

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
                (range from (inc to)))))
       (into [])))

(defn keyset [keys]
  (if (= 1 (count keys))
    (first keys)
    (let [{ints true keys false} (group-by integer? keys)
          keys (cond-> (into #{} keys)
                       (seq ints) (into (ranges ints)))]
      (into [] keys))))

(defn keyset? [x]
  (or (key? x)
      (range? x)
      (and (vector? x)
           (every? #(or (key? %) (range? %)) x))))

(defn keys [x]
  (cond
    (key? x) [x]
    (range? x) (range-keys x)
    (vector? x) (mapcat keys x)))

(defn keyset-seq [x]
  (cond
    (key? x) [x]
    (range? x) [x]
    (vector? x) x))

;;; PATHS

(defn path? [x]
  (and (vector? x)
       (every? key? x)))

(defn pathset? [x]
  (and (vector? x)
       (every? keyset? x)))

(defn expand-pathset
  [[keyset & pathset]]
  (if (seq pathset)
    (for [key (keys keyset)
          path (expand-pathset pathset)]
      (into [key] path))
    (map vector (keys keyset))))

(defn expand-pathset'
  [pathset]
  (letfn [(inner-expand [path [keyset & pathset]]
            (let [paths (map #(conj path %) (keys keyset))]
              (if (seq pathset)
                (mapcat #(inner-expand % pathset) paths)
                paths)))]
    (inner-expand [] pathset)))

(defn expand-pathset''
  [pathset]
  (letfn [(walk-pathset [expansions path [keyset & pathset]]
            (let [paths (into [] (map #(conj path %)) (keys keyset))]
              (if (seq pathset)
                (reduce (fn [expansions path]
                          (walk-pathset expansions path pathset))
                        expansions
                        paths)
                (reduce (fn [expansions path]
                          (conj! expansions path))
                        expansions
                        paths))))]
    (persistent! (walk-pathset (transient []) [] pathset))))

(defn expand-pathset'''
  [pathset]
  (letfn [(pathset-seq [path [keyset & pathset]]
            (let [paths (into [] (map #(conj path %)) (keys keyset))]
              (if (seq pathset)
                (mapcat #(pathset-seq % pathset) paths)
                paths)))]
    (into [] (pathset-seq [] pathset))))

(defn expand-pathsets [pathsets]
  (into [] (mapcat expand-pathset''' pathsets)))

(def leaf ::leaf)
(defn leaf? [v] (= leaf v))

(defn pathsets [tree]
  (letfn [(inner-pathsets [path tree]
            (mapcat (fn [[sub-tree kvs]]
                      (let [path (conj path (keyset (map first kvs)))]
                        (if (leaf? sub-tree)
                          [path]
                          (inner-pathsets path sub-tree))))
                    (group-by second tree)))]
    (inner-pathsets [] tree)))

(defn length-tree'
  [length pathsets]
  (letfn [(keys [keyset]
            (if (vector? keyset)
              keyset
              [keyset]))
          (pathset-tree [tree [ks & pathset]]
            (if (seq pathset)
              (reduce (fn [tree k]
                        (update tree k pathset-tree pathset))
                      tree
                      (keys ks))
              (reduce (fn [tree k]
                        (assoc tree k leaf))
                      tree
                      (keys ks))))]
    [length (reduce pathset-tree {} pathsets)]))

(defn length-tree
  [pathsets]
  (into {}
        (map (fn [[length tree]] (length-tree' length tree)))
        (group-by count pathsets)))

(defn length-tree-pathsets
  [length-tree]
  (mapcat (comp pathsets second) length-tree))

(defn tree [pathsets]
  (loop [[p & ps] (expand-pathsets pathsets) tree {}]
    (if p
      (recur ps (assoc-in tree p leaf))
      tree)))

(defn collapse [pathsets]
  (length-tree-pathsets (length-tree pathsets)))

(defn assert-inner-reference
  [cache ref]
  (loop [cache cache [key & rest] ref]
    (if (ref? cache)
      (throw (ex-info "inner reference" {:path ref}))
      (when (seq rest)
        (recur (clojure.core/get cache key) rest)))))

;; TODO: redo this ...
(defn optimize* [root cache [key & rest :as path] optimized]
  (cond
    (ref? cache) (let [ref-path (:path cache)]
                   (assert-inner-reference root ref-path)
                   (if (seq? path)
                     (optimize* root root (concat ref-path path) [])
                     [ref-path]))

    (atom? cache) []
    :else (mapcat (fn [key]
                    (if (map? cache)
                      (if (contains? cache key)
                        (optimize* root (clojure.core/get cache key) rest (conj optimized key))
                        [(into [] (concat optimized [key] rest))])
                      []))
                  (keys key))))

(defn optimize
  [cache paths]
  (mapcat (fn [path]
            (optimize* cache cache path []))
          paths))

(defn pathmap-paths
  [pathmap]
  (letfn [(paths [path pathmap]
            (if (and (map? pathmap)
                     (not (boxed? pathmap)))
              (mapcat (fn [[key pathmap]]
                        (paths (conj path key)
                               pathmap))
                      pathmap)
              [path]))]
    (paths [] pathmap)))

(defn branch? [x]
  (and (map? x)
       (not (boxed? x))))

(defn pathmap-values
  [pathmap]
  (letfn [(path-values [path pathmap]
            (if (branch? pathmap)
              (mapcat (fn [[key pathmap]]
                        (path-values (conj path key)
                                     pathmap))
                      pathmap)
              [(path-value path pathmap)]))]
    (path-values [] pathmap)))

(defn join?
  [x]
  (and (map? x)
       (= 1 (count x))
       (vector? (second (first x)))))

(defn query?
  [x]
  (vector? x))

(defn prepend-query
  ([path]
   (prepend-query path nil))
  ([[k & path] query]
   (cond
     (seq path) [{k (prepend-query path query)}]
     (seq query) [{k query}]
     :else [k])))

;; TODO: this has proved useful -- make it robust!
(defn query-pathsets [query]
  (letfn [(create-range [min max]
            (cond
              ;; TODO: replace with a range/max when we got that
              (and (nil? min) (nil? max)) (range 0 100)
              (and (nil? max)) (range 0 min)
              :else (range min max)))
          (expand-ranges [key]
            (cond
              (list? key)
              (let [[name key min max] key]
                (assert (= name 'range))
                {key [(create-range min max)]})

              (and (map? key)
                   (list? (ffirst key)))
              (let [[[name key min max] query] (first key)]
                (assert (= name 'range))
                {key [{(create-range min max) query}]})

              :else key))]
    (let [query (map expand-ranges query)
          leafs (into [] (remove map? query))
          paths (for [join (filter map? query)
                      :let [[key query] (first join)]
                      paths (query-pathsets query)]
                  (into [key] paths))]
      (cond-> (into [] paths)
              (seq leafs) (conj [leafs])))))

(defn eval-query
  [query]
  (letfn [(expand-range [[name key min max] query]
            (assert (= name 'range))
            (let [range (cond
                          (and (nil? min) (nil? max)) (range 0 100)
                          (and (nil? max)) (range 0 min)
                          :else (range min max))]
              (if (seq query)
                {key [{range query}]}
                {key [range]})))]
    (into []
          (map (fn [key]
                 (cond
                   (list? key) (expand-range key nil)
                   (join? key) (let [[key query] (first key)]
                                 (if (list? key)
                                   (expand-range key (eval-query query))
                                   {key (eval-query query)}))
                   :else key)))
          query)))

(defn tree-query
  [tree]
  (reduce (fn [query [key tree]]
            (if (leaf? tree)
              (conj query key)
              (conj query {key (tree-query tree)})))
          []
          tree))

(defn pathsets-query [pathsets]
  (letfn [(keys [keyset]
            (if (vector? keyset)
              keyset
              [keyset]))
          (merge-query-tree [tree [ks & pathset]]
            (if (seq pathset)
              (reduce (fn [tree k]
                        (update tree k merge-query-tree pathset))
                      tree
                      (keys ks))
              (reduce (fn [tree k]
                        (assoc tree k leaf))
                      tree
                      (keys ks))))]
    (tree-query (reduce merge-query-tree
                        {}
                        pathsets))))
