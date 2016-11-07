(ns generator.parse-tree
  (:require [clojure.test :as t]
            [clojure.zip :as z]
            [generator.test :as tt]))

(def examples
  ['({:token :章 :value 1}
     ({:token :节 :value 1}
      ({:token :条 :value 1}
       {:token :undefined :value "separator"})
      ({:token :条 :value 2}))
     ({:token :节 :value 2}))

   '({:token :法}
     ({:token :条}
      {:token :separator}
      ({:token :款 :nth 1}
       ({:token :项}))))])

(def create
  (partial z/zipper
           seq? ;branch?
           rest ;children
           (fn [x children] ;makenode
             (cons (first x) children))))

(def ^:private branch list)

(defn node-val [loc]
  (let [x (z/node loc)]
    (cond-> x
      (seq? x) first)))

(defn subtrees [loc]
  (->> loc
       z/down
       (iterate z/right)
       (take-while identity)))

(defn internal-node? [loc]
  (and (z/branch? loc) (seq (z/children loc))))

(defn- climb [loc f & args]
  (loop [loc loc]
    (if-let [r (apply f loc args)]
      r
      (recur (z/up loc)))))

(defn- append-child-mv [loc node-x]
  (-> loc
      (z/append-child node-x)
      z/down
      z/rightmost))

(defn- insert-right-mv [loc node-x]
  (-> loc
      (z/insert-right node-x)
      z/right))

(defn- move-to-root
  {:test
   #(let [f move-to-root]
      (tt/comprehend-tests
       (t/is (nil? (f nil)))
       (let [x (create :node)]
         (t/is (= x (f x))))
       (let [x (create (branch :root))]
         (t/is (= (node-val x)
                  (node-val (f (append-child-mv x :node))))))))}
  [loc]
  (let [no-parent? (comp nil? z/up)]
    (and loc (climb loc
                    #(when (no-parent? %) %)))))

(defn hierachy-fn [hval]
  (fn [tx] ((:token tx) hval)))

(def doc-hierachy
  (hierachy-fn {:目 1 :项 2 :款 3 :条 4 :节 5 :章 6 :则 7 :编 7
                :法 10 :规定 10 :办法 10}))

(def 款-filler {:token :款 :nth 1})

(defn- fill-the-path [fillers h hier-high-exclusive x]
  (let [h-inv (into (sorted-map)
                    (map (fn [t] [(h t) t]) fillers))
        hier-x (h x)]
    (assert (number? hier-x))
    (rseq
     (into [x]
           (->> h-inv
                (drop-while #(<= (key %) hier-x))
                (take-while #(< (key %) hier-high-exclusive))
                (map val))))))

(defn linear-to-tree
  {:test
   #(let [f linear-to-tree]
      (tt/comprehend-tests
       (t/is (= () (f () doc-hierachy)))
       (for [t examples
             :let [l (flatten t)]]
         (t/is (= t (f l doc-hierachy [款-filler]))))))}
  ([xs h] (linear-to-tree xs h {}))
  ([xs h fillers]
   (if (empty? xs)
     ()
     (let [fill (partial fill-the-path fillers h)

           absorb
           (fn [loc x]
             (let [hier-x (h x)]
               (if (nil? hier-x)
                 ;; possibly, a separator
                 ;; IMPORTANT! Use x instead of (branch x),
                 ;; so that it wouldn't be a parent node.
                 (z/append-child loc x)

                 (let [hier (h (node-val loc))
                       path (fill hier x)]
                   (cond (< hier-x hier)
                         (reduce
                          (fn [loc val]
                            (append-child-mv loc (branch val)))
                          loc path)

                         (= hier-x hier)
                         (insert-right-mv loc (branch x)))))))]
       (loop [loc (create (branch (first xs)))
              rxs (rest xs)]
         (if (empty? rxs)
           (z/root loc)
           (recur (climb loc absorb (first rxs))
                  (rest rxs))))))))

(defn update-leaves
  {:test
   #(let [f update-leaves
          t1 (create (linear-to-tree () doc-hierachy))
          t2 (create (nth examples 1))
          pf count
          k  :depth]
      (tt/comprehend-tests
       (t/is (= '({:depth 1}) (f t1 k pf)))
       (t/is (= '({:token :法}
                  ({:token :条}
                   {:token :separator}
                   ({:token :款 :nth 1}
                    ({:token :项 :depth 4}))))
                (f t2 k pf)))))}
  [loc k f]
  (letfn [(sep-token? [t]
            (and (map? t) (= (:token t) :separator)))
          (leaf? [loc]
            (or (not (z/branch? loc))
                (or (empty? (z/children loc))
                    (and (every? map? (z/children loc))
                         (every? sep-token? (z/children loc))))))
          (ret-v [f loc]
            (f (conj (vec (map first (z/path loc)))
                     (node-val loc))))]
    (if (z/end? loc)
      (z/root loc)
      (cond
        (sep-token? (z/node loc))
        (recur (z/next loc) k f)

        (nil? (z/node loc))
        (recur (z/next loc) k f)

        (not (z/branch? loc))
        (recur (z/next
                (z/edit loc #(assoc % k (ret-v f loc)))) k f)

        (or (empty? (z/children loc))
            (every? sep-token? (z/children loc)))
        (recur (z/next
                (z/edit loc #(cons (assoc (first %) k (ret-v f loc))
                                   (z/children loc))))
               k f)

        :else
        (recur (z/next loc) k f)))))
