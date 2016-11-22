(ns generator.toc
  (:require [clojure.string :as str]
            [clojure.test :as t]
            [generator.id :as id]
            [generator.line :as ln]
            [generator.lisp :as s]
            [generator.parse-tree :as pt]
            [generator.test :as tt]))

(defn outline-html [h]
  (letfn [(li [t]
            (let [hash   (str "#" (id/entry-id (:context t) (:token t)))
                  elmt-a [:a {:href hash} (:text t)]]
              [:li
               [:div {:class "li-head"}
                elmt-a
                (when-let [r (:entries-range t)]
                  (let [a (int (:from r))
                        b (int (:to r))]
                    (t/is (<= a b))
                    [:span (str "条" a
                                (when (< a b) (str "-" b)))]))]]))
          (to-html [ot]
            (let [t (pt/node-val ot)
                  r (when (pt/internal-node? ot)
                      [:ul (for [li (pt/subtrees ot)]
                             (to-html li))])]
              (cond->> r
                (seq (:text t)) (conj (li t)))))]
    (to-html (pt/create h))))

(def empty-toc {:token :table-of-contents :not-in-original-text true})

(defn generate-table-of-contents
  {:test
   #(let [f generate-table-of-contents
          tls (ln/draw-skeleton ["前言" "第一章" "a"
                                 "第二章" "b"
                                 "第三章" "第一节" "第二条……"])]
      (tt/comprehend-tests
       (t/is (= [() empty-toc] (f ())))
       (t/is (= [["前言"] empty-toc]
                (->> [(map :text prelude) r]
                     (let [[prelude r] (f (take 1 tls))]))))
       (t/is (= {:token :table-of-contents
                 :text "目录"
                 :list '({:token :pseudo-root}
                         ({:token :章, :nth 1, :text "第一章"})
                         ({:token :章, :nth 2, :text "第二章"})
                         ({:token :章, :nth 3, :text "第三章"}
                          {:token :节, :nth 1, :text "第一节"
                           :entries-range {:from 2 :to 2}}))
                 :not-in-original-text true}
                (second (f tls))))))}
  [tls]
  (letfn [(max-hier [hval ts]
            (apply max (remove nil? (map (pt/hierachy-fn hval) ts))))
          (rise-ts [hval ts]
            (pt/linear-to-tree
             (cons {:token :pseudo-root} ts)
             (pt/hierachy-fn
              ;;hier val of :序言 is determined dynamically
              (merge hval {:序言 (max-hier hval ts)
                           :pseudo-root (inc (apply max (vals hval)))}))))
          (digest-条s-in-hier [h]
            (let [node first
                  v (node h)
                  children (rest h)
                  flag #(= (:token %) :条)
                  digest (fn [es] {:from (:nth (first es))
                                   :to (apply max (map :nth es))})]
              (if (empty? children)
                h
                (let [cvs (map node children)]
                  (if (every? flag cvs)
                    (assoc v :entries-range (digest cvs))
                    (cons v (map digest-条s-in-hier children)))))))]
   (let [[prelude tls-initial]
         (split-with #(= (:token %) :to-be-recognized) tls)

         tls-prepare
         (filter #(#{:序言 :编 :则 :章 :节 :条} (:token %)) tls-initial)]
     [prelude (if (empty? (remove #(= (:token %) :条) tls-prepare))
                empty-toc
                {:token :table-of-contents
                 :text "目录"
                 :list (digest-条s-in-hier
                        (rise-ts {:条 0 :节 1 :章 2 :则 3 :编 3}
                                 tls-prepare))
                 :not-in-original-text true})])))

(def table-of-contents-sentinel #"目\s*录")
(defn table-of-contents
  {:test
   #(let [txt ["目 录" "第一章" "第二章" "第三章" "第一章" "……"]]
      (tt/comprehend-tests
       [(t/is (= [["目 录"]
                  {:token :table-of-contents
                   :text "目 录"
                   :list []}]
                 (table-of-contents ["目 录"])))
        (t/is (= [["目 录" "第一章" "第二章" "第三章"]
                  {:token :table-of-contents
                   :text "目 录"
                   :list ["第一章" "第二章" "第三章"]}]
                 (table-of-contents txt)))]))}
  [ls]
  (let [head (first ls)
        equal-without-spaces (fn [s t]
                               (= (str/join (str/split s #"\s"))
                                  (str/join (str/split t #"\s"))))]
    (assert (re-matches table-of-contents-sentinel head))
    (if-let [first-item (second ls)]
      (loop [s (rest (rest ls))
             t [first-item]]
        (if (or (equal-without-spaces (first s) first-item) (empty? s))
          [(cons head t) {:token :table-of-contents :text head :list t}]
          (recur (rest s) (conj t (first s)))))
      [[head] {:token :table-of-contents :text head :list []}])))

(defn recognize-table-of-contents
  {:test
   #(let [f recognize-table-of-contents
          txt ["前言" "目 录" "第一章" "第二章" "第三章" "第一章" "……"]]
      (tt/comprehend-tests
       [(t/is (= [[] []] (f ["第一章"])))
        (t/is (= [[{:token :table-of-contents :text "目 录" :list []}] []]
                 (f ["目 录"])))
        (t/is (= [[{:token :to-be-recognized :text "前言"}
                   {:token :table-of-contents :text "目 录"
                    :list ["第一章" "第二章" "第三章"]}]
                  ["第一章" "……"]]
                 (f txt)))]))}
  ([ls] (recognize-table-of-contents ls []))
  ([ls ts]
   (if (empty? ls)
     [[] []]
     (let [l (first ls)]
       (if (re-matches table-of-contents-sentinel l)
         (let [[p t] (table-of-contents ls)]
           [(conj ts t) (s/without-prefix ls p)])
         (recur (rest ls)
                (conj ts {:token :to-be-recognized :text l})))))))
