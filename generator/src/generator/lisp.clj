(ns generator.lisp
  (:require [clojure.test :as t]
            [clojure.zip :as z]
            [generator.test :as tt]))

(defn without-prefix [origin prefix]
  (loop [s origin t prefix]
    (cond
      (empty? t) s
      (not= (first s) (first t)) origin
      :else (recur (rest s) (rest t)))))

(defn read-xs [from to cs]
  (when-let [[begin body] (from cs)]
    (when-let [[body end] (to body)]
      [begin body end])))

(defn from-x
  {:test
   #(let [f (partial from-x \第)]
      (tt/comprehend-tests
       (t/is (nil? (f [])))
       (t/is (nil? (f [\newline \第])))
       (t/is (= [[\第] [\a \b \c]] (f [\第 \a \b \c])))))}
  [beginc [c & cs]]
  (when (= c beginc) [[c] cs]))

(defn to-x
  {:test
   #(let [f (partial to-x \条)]
      (tt/comprehend-tests
       (t/is (nil? (f [])))
       (t/is (nil? (f [\a \b])))
       (t/is (= [[\a \b] [\条]] (f [\a \b \条])))))}
  [endc cs]
  (loop [body []
         [end & tail] cs]
    (if (= end endc)
      [body [end]]
      (when tail
        (recur (conj body end) tail)))))

(defn flatten-and-vector
  {:test
   #(let [f flatten-and-vector]
      (tt/comprehend-tests
       (t/is (= [] (f)))
       (t/is (= [1 2 3] (f [1] [2 3])))
       (t/is (= [1 2 3 4] (f (f [1] [2 [3]]) [4])))))}
  ([] [])
  ([xs] (into [] (flatten xs)))
  ([xs & more-xss] (into (flatten-and-vector xs)
                         (apply flatten-and-vector more-xss))))

(defn seq-match
  {:test
   #(let [f (partial seq-match
                     [#{[\本]}
                      #{[\规 \定] [\法] [\条]}
                      #{[\第]}])]
      (tt/comprehend-tests
       (t/is (f (seq "本规定第一条")))
       (t/is (f (seq "本法第二条")))
       (t/is (f (seq "本条第一项")))
       (t/is (not (f (seq "本款地三项"))))
       (t/is (not (f (seq "前条第二款"))))
       (t/is (not (f (seq "本规定情况下"))))))}
  [word-sets cs]
  (first
   (reduce
    (fn [[_ cs] word-set]
      (if-let [word (some seq
                          (for [word word-set]
                            (when (= word (take (count word) cs))
                              word)))]
        [true (without-prefix cs word)]
        (reduced [false cs])))
    [true cs] word-sets)))

(defn map-on-binary-partitions
  {:test
   #(let [f map-on-binary-partitions]
      (tt/comprehend-tests
       (t/is (= [-2 0 -4]
                (f pos? [-1 -1 1 1 -1 -1 -1 -1]
                   (partial apply -) (partial apply +))))))}
  [pred coll yes-fn no-fn]
  (->> (partition-by pred coll)
       (map #((let [representative (first %)]
                (if (pred representative)
                  yes-fn
                  no-fn))
              %))))

(def tree (partial z/zipper
                   seq? ;branch?
                   rest ;children
                   (fn [x children] ;makenode
                     (cons (first x) children))))

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
     (let [node     list
           node-val (comp first z/node)
           root     (fn [loc]
                      (last
                       (take-while identity (iterate z/up loc))))
           h-inv    (when fill
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
                                   (-> loc
                                       (z/append-child (node val))
                                       z/down
                                       z/rightmost))
                                 loc path)

                                (= x-hier hier)
                                (-> loc
                                    (z/insert-right (node x))
                                    z/right)

                                :climb
                                (recur (z/up loc) x)))
                        ;; possibly, a separator
                        ;; IMPORTANT! Use x instead of (node x),
                        ;; so that it wouldn't be a parent node.
                        (z/append-child loc x)))]
       (loop [loc (tree (node (first xs)))
              rxs (rest xs)]
         (if (empty? rxs)
           (z/node (root loc))
           (recur (absorb loc (first rxs)) (rest rxs))))))))
