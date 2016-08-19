(ns generator.line
  (:require [clojure.string :as str]
            [clojure.test :as t]
            [generator.lisp :as s]
            [generator.test :as tt]
            [generator.zh-digits :refer [数字 numchar-zh-set]]))

(defn nth-item
  {:test
   #(let [f nth-item]
      (tt/comprehend-tests
       (t/is (= [1 :章 (seq "第一章")] (f "第一章 总则")))
       (t/is (= [12 :条 (seq "第十二条")] (f "第十二条 ……")))))}
  [line]
  (let [[c & cs] line]
    (when (= c \第)
      (let [[i processed] (数字 cs)
            tailc (first (s/without-prefix cs processed))]
        [i
         (keyword (str tailc))
         (-> [c] (into processed) (conj tailc))]))))

(defn 括号数字
  {:test
   #(let [f 括号数字]
      (tt/comprehend-tests
       (t/is (not (f "（")))
       (t/is (not (f "（）")))
       (t/is (not (f "（括号内）")))
       (t/is (= [1 [\（ \一 \）]] (f "（一）……")))))}
  [cs]
  (let [openfn (partial s/from-x \（)
        closefn (partial s/to-x \）)]
    (when-let [[begin body end] (s/read-xs openfn closefn cs)]
      (let [[i processed] (数字 body)]
        (when (and (seq processed) (= processed body))
          [i (s/flatten-and-vector begin body end)])))))

(def table-of-contents-sentinel #"目\s*录")
(defn table-of-contents
  {:test
   #(let [txt ["目 录" "第一章" "第二章" "第三章" "第一章" "……"]]
      (tt/comprehend-tests
       [(t/is (= [["目 录"]
                  {:token :table-of-contents
                   :list ["目 录"]}]
                 (table-of-contents ["目 录"])))
        (t/is (= [["目 录" "第一章" "第二章" "第三章"]
                  {:token :table-of-contents
                   :list ["目 录" "第一章" "第二章" "第三章"]}]
                 (table-of-contents txt)))]))}
  [ls]
  (let [head (first ls)
        equal-without-spaces (fn [s t]
                               (= (str/join (str/split s #"\s"))
                                  (str/join (str/split t #"\s"))))]
    (assert (re-matches table-of-contents-sentinel head))
    (if-let [first-item (second ls)]
      (loop [s (rest (rest ls))
             t [head first-item]]
        (if (or (equal-without-spaces (first s) first-item) (empty? s))
          [t {:token :table-of-contents :list t}]
          (recur (rest s) (conj t (first s)))))
      [[head] {:token :table-of-contents :list [head]}])))

(defn recognize-table-of-contents
  {:test
   #(let [f recognize-table-of-contents
          txt ["前言" "目 录" "第一章" "第二章" "第三章" "第一章" "……"]]
      (tt/comprehend-tests
       [(t/is (= [[] []] (f ["第一章"])))
        (t/is (= [[{:token :table-of-contents :list ["目 录"]}] []]
                 (f ["目 录"])))
        (t/is (= [[{:token :to-be-recognized :text "前言"}
                   {:token :table-of-contents
                    :list ["目 录" "第一章" "第二章" "第三章"]}]
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

(defn draw-skeleton
  {:test
   #(let [f draw-skeleton
          lines ["第一章 abc" "第一条 cde" "fgh" "（一）ijk"
                 "（二）lmn" "opq" "第二条 rst"]
          r (f lines)]
      (tt/comprehend-tests
       (t/is (= {:token :章 :nth 1 :text "第一章 abc"} (nth r 0)))
       (t/is (= {:token :条 :nth 1 :text "第一条"}     (nth r 1)))
       (t/is (= {:token :款 :nth 1 :text "cde"}        (nth r 2)))
       (t/is (= {:token :款 :nth 2 :text "fgh"}        (nth r 3)))
       (t/is (= {:token :项 :nth 1 :text "（一）ijk"}  (nth r 4)))
       (t/is (= {:token :项 :nth 2 :text "（二）lmn"}  (nth r 5)))
       (t/is (= {:token :款 :nth 3 :text "opq"}        (nth r 6)))
       (t/is (= {:token :条 :nth 2 :text "第二条"}     (nth r 7)))
       (t/is (= {:token :款 :nth 1 :text "rst"}        (nth r 8)))))}
  [lines]
  (let [new-ret (fn [ret t l & i]
                  (cond-> (-> ret
                              (update :lines conj t)
                              (assoc-in [:env :level] l))
                    (first i) (assoc-in [:env :i-款] (first i))))]
   (map
    #(update % :text str/join)
    (:lines
     (reduce
      (fn [{env :env :as ret} line]
        (if-let [[i unit processed] (nth-item line)]
          (cond (#{:章 :节} unit)
                (new-ret ret {:token unit :nth i :text line} unit)

                (= unit :条)
                (recur
                 (new-ret ret {:token unit :nth i :text processed} :款 0)
                 (rest (s/without-prefix line processed))))
          (apply new-ret ret
                 (if (#{:款 :项} (:level env))
                   (if-let [[i _] (括号数字 line)]
                     [{:token :项 :nth i :text line} :项]
                     (let [i (inc (:i-款 env))]
                       [{:token :款 :nth i :text line} :款 i]))
                   [{:token :to-be-recognized :text line} nil]))))
      {:env {:level nil :i-款 nil} :lines []}
      lines)))))

(defn attach-table-of-contents
  {:test
   #(let [f attach-table-of-contents
          tls (draw-skeleton ["前言" "第一章" "a"
                              "第二章" "b"
                              "第三章" "第一节" "……"])]
      (tt/comprehend-tests
       (t/is (= [] (f ())))
       (t/is (= ["前言"] (map :text (f (take 1 tls)))))
       (t/is (= {:token :table-of-contents
                 :list ["目录" "第一章" "第二章" "第三章" "第一节"]}
                (second (f tls))))))}
  [tls]
  (let [[prelude tls'] (split-with #(= (:token %)
                                       :to-be-recognized) tls)
        titles (->> tls'
                    (filter #(#{:章 :节} (:token %)))
                    (map :text))]
    (cond-> (vec prelude)
      (seq titles) (conj {:token :table-of-contents
                          :list (cons "目录" titles)})
      true (into tls'))))
