(ns generator.tree
  (:require [clojure.test :as t]
            [clojure.zip :as z]
            [generator.test :as tt]))

(def create (partial z/zipper
                   seq? ;branch?
                   rest ;children
                   (fn [x children] ;makenode
                     (cons (first x) children))))

(def branch list)

(def node-val (comp first z/node))

(defn climb [loc f & args]
  (loop [loc loc]
    (if-let [r (apply f loc args)]
      r
      (recur (z/up loc)))))

(defn update-leaves
  ([tree k f] (update-leaves tree [] k f))
  ([[parent & children] path k f]
   (let [path' (conj path parent)]
     (if (or (empty? children)
             (every? #(= (:token %) :separator) children))
       (cons (assoc parent k (f path')) children)
       (cons parent (map #(if (= (:token %) :separator)
                            %
                            (update-leaves % path' k f)) children))))))

(defn- add-child-and-move-to [loc node-x]
  (-> loc
      (z/append-child node-x)
      z/down
      z/rightmost))

(defn- add-sibling-and-move-to [loc node-x]
  (-> loc
      (z/insert-right node-x)
      z/right))

(defn move-to-root
  {:test
   #(tt/comprehend-tests
     (t/is (nil? (root nil)))
     (let [x (create :node)]
       (t/is (= x (root x))))
     (let [x (create (branch :root))]
       (t/is (= (node-val x)
                (node-val (root
                           (add-child-and-move-to x :node)))))))}
  [loc]
  (let [no-parent? (comp nil? z/up)]
    (and loc (climb loc
                    #(when (no-parent? %) %)))))

(defn linear-to-tree
  {:test
   #(let [f linear-to-tree
          hier (fn [x]
                 (let [h {:法 10 :章 5 :节 4 :条 3 :款 2 :项 1}]
                   (h (:token x))))]
      (tt/comprehend-tests
       (t/is (= () (f () hier)))
       (t/is (= '({:token :章 :value 1}
                  ({:token :节 :value 1}
                   ({:token :条 :value 1}
                    {:token :undefined :value "separator"})
                   ({:token :条 :value 2}))
                  ({:token :节 :value 2}))
                (f [{:token :章 :value 1}
                    {:token :节 :value 1}
                    {:token :条 :value 1}
                    {:token :undefined :value "separator"}
                    {:token :条 :value 2}
                    {:token :节 :value 2}] hier)))
       (t/is (= '({:token :法}
                  ({:token :条}
                   {:token :separator}
                   ({:token :款 :nth 1}
                    ({:token :项}))))
                (f [{:token :法} {:token :条} {:token :separator}
                    {:token :项}]
                   hier {{:token :款} {:token :款 :nth 1}})))))}
  ([xs h] (linear-to-tree xs h {}))
  ([xs h fill]
   (if (empty? xs)
     ()
     (let [h-inv    (when fill
                      (reduce
                       (fn [ret [k v]]
                         (conj ret [(h k) v]))
                       (sorted-map) (sort-by (comp h key) fill)))
           absorb   (fn [loc x]
                      (if-let [x-hier (h x)]
                        (let [hier (h (node-val loc))
                              path (if (empty? fill)
                                     [x]
                                     (conj
                                      (mapv val
                                            (vec
                                             (->> h-inv
                                                  rseq
                                                  (drop-while
                                                   #(>= (key %) hier))
                                                  (take-while
                                                   #(> (key %) x-hier)))))
                                      x))]
                          (cond (< x-hier hier)
                                (reduce
                                 (fn [loc val]
                                   (add-child-and-move-to loc (branch val)))
                                 loc path)

                                (= x-hier hier)
                                (add-sibling-and-move-to loc (branch x))))
                        ;; possibly, a separator
                        ;; IMPORTANT! Use x instead of (branch x),
                        ;; so that it wouldn't be a parent node.
                        (z/append-child loc x)))]
       (loop [loc (create (branch (first xs)))
              rxs (rest xs)]
         (if (empty? rxs)
           (z/root loc)
           (recur (climb loc absorb (first rxs)) (rest rxs))))))))
