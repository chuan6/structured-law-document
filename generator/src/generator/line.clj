(ns generator.line
  (:require [clojure.string :as str]
            [clojure.test :as t]
            [generator.lisp :as s]
            [generator.parse-tree :as pt]
            [generator.punct :as punct]
            [generator.test :as tt]
            [generator.zh-digits :refer [数字 numchar-zh-set]]))

(def sub-inc (partial + 1/1000))

(defn nth-item
  {:test
   #(let [f nth-item]
      (tt/comprehend-tests
       (t/is (= [1 :章 (seq "第一章")] (f "第一章 总则")))
       (t/is (= [12 :条 (seq "第十二条")] (f "第十二条 ……")))
       (t/is (= [1001/1000 :条 (seq "第一条之一")] (f "第一条之一……")))
       (t/is (= [1005/1000 :条 (seq "第一条之五")] (f "第一条之五……")))
       (t/is (= [1015/1000 :条 (seq "第一条之十五")] (f "第一条之十五……")))))}
  [line]
  (letfn [(add-sub [base times]
            (nth (iterate sub-inc base) times))]
    (let [[c & cs] line]
      (when (= c \第)
        (let [[i processed] (数字 cs)
              ;;expect unit to be single-character
              [c1 c2 & cmore] (s/without-prefix cs processed)
              ty (keyword (str c1))]
          (if (and (= ty :条) (= c2 \之))
            (let [[i' processed'] (数字 cmore)]
              (if (empty? processed')
                [i ty (s/flatten-and-vector [c] processed [c1])]
                [(add-sub i i')
                 ty
                 (s/flatten-and-vector [c] processed [c1 c2] processed')]))
            [i ty (s/flatten-and-vector [c] processed [c1])]))))))

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

(defn digits
  {:test
   #(let [f digits]
      (tt/comprehend-tests
       (t/is (nil?       (f "1x")))
       (t/is (= [1 [\x]] (f "1.x")))
       (t/is (= [1 [\x]] (f "1．x")))
       (t/is (= [1 [\x]] (f "1. x")))))}
  [cs]
  (let [[ds cs] (split-with (set (seq "0123456789")) cs)
        sep-set #{\space \.
                  \．; U+FF0E, fullwidth full stop
                  }
        cs' (drop-while sep-set cs)]
    (when (and (seq ds) (sep-set (first cs)) (seq cs'))
      [(Integer/parseInt (str/join ds)) cs'])))

(defn 则
  {:test
   #(let [f 则]
      (tt/comprehend-tests
       (t/is (nil? (f ())))
       (t/is (nil? (f (seq "abc"))))
       (t/is (nil? (f (seq "d则"))))
       (t/is (nil? (f (seq "总则x"))))
       (t/is (= [:general (seq "总则")] (f (seq "总则"))))
       (t/is (= [:special (seq "分 则")] (f (seq "分 则"))))
       (t/is (= [:supplementary (seq "附 则")] (f (seq "附 则 efg"))))))}
  [char-seq]
  (if (empty? char-seq)
    nil
    (let [xs (list :nth :space :unit)
          ct {\总 :general \分 :special \附 :supplementary}
          next-step (fn [ret x]
                      (-> ret
                          (update :text conj x)
                          (update :steps pop)))
          result (reduce (fn [ret x]
                           (case (peek (:steps ret))
                             :nth   (if (#{\总 \分 \附} x)
                                      (-> (assoc ret :nth (ct x))
                                          (next-step x))
                                      (reduced nil))

                             :space (if (= x \space)
                                      (next-step ret x)
                                      (recur (update ret :steps pop) x))

                             :unit  (if (= x \则)
                                      (reduced (next-step ret x))
                                      (reduced nil))))
                         {:nth nil :text [] :steps xs} char-seq)
          c (first (s/without-prefix char-seq (:text result)))]
      (when (and result (or (nil? c) (= c \space)))
        [(:nth result) (:text result)]))))

(defn recognize-title
  {:test
   #(let [f recognize-title
          txt ["前言" "这是标题" "目录" "……"]]
      (tt/comprehend-tests
       (t/is (= [[] []] (f (take 1 txt) "标题")))
       (t/is (= {:token :title :head "这是" :text "标题"}
                (first (first (f (rest txt) "标题")))))
       (t/is (= [[{:token :to-be-recognized :text (first txt)}
                  {:token :title :head "这是" :text "标题"}]
                 (rest (rest txt))]
                (let [[ts ls] (f txt "标题")]
                  [(take 2 ts) ls])))))}
  ([ls s] (recognize-title ls [] s))
  ([ls ts s]
   (letfn [(cut-suffix [line sf]
             (str/join (take (- (count line) (count sf)) line)))]
     (if (empty? ls)
       [[] []]
       (let [l (first ls)]
         (if (and (seq s) (.endsWith l s))
           [(conj ts {:token :title
                      :head (cut-suffix l s)
                      :text s})
            (rest ls)]
           (recur (rest ls)
                  (conj ts {:token :to-be-recognized :text l})
                  s)))))))

(defn- prefixed-line-token
  {:test
   #(let [f prefixed-line-token]
      (tt/comprehend-tests
       (t/is (= {:token :序言 :text "序 言"}      (f "序 言")))
       (t/is (= {:token :编 :nth 1
                 :text "第一编 总 则"}            (f "第一编 总 则")))
       (t/is (= {:token :则 :nth :general
                 :text "总则"}                    (f "总则")))
       (t/is (= {:token :章 :nth 1
                 :text "第一章 abc"}              (f "第一章 abc")))
       (t/is (= [{:token :条 :nth 1 :text "第一条"}
                 {:token :款 :nth 1 :text "cde"}] (f "第一条 cde")))
       (t/is (= {:token :to-be-recognized
                 :text "第三人fgh"}               (f "第三人fgh")))
       (t/is (= {:token :项 :nth 1
                 :text "（一）ijk"}               (f "（一）ijk")))
       (t/is (= {:token :目 :nth 1
                 :text "x"}                       (f "1.x")))
       (t/is (= [{:token :条 :nth 2
                  :text "第二条"}]                (f "第二条")))
       (t/is (= {:token :则 :nth :special
                 :text "分 则"}                   (f "分 则")))))}
  [line]
  (assert (string? line))
  (let [skip-?-space
        (fn [cs]
          (cond-> cs
            (= (first cs) \space) rest))]
    (or
     (let [[i unit processed] (nth-item line)]
       (cond
         (#{:编 :章 :节} unit)
         {:token unit :nth i :text line}

         (= unit :条)
         (let [first-款 (skip-?-space
                         (s/without-prefix line processed))]
           (cond-> [{:token :条 :nth i :text (str/join processed)}]
             (seq first-款) (conj {:token :款 :nth 1
                                   :text (str/join first-款)})))))
     (when-let [[i _] (则 line)]
       {:token :则 :nth i :text line})
     (when-let [[i _] (括号数字 line)]
       {:token :项 :nth i :text line})
     (when-let [[i line'] (digits line)]
       {:token :目 :nth i :text (str/join line')})
     (when (re-matches #"序\s*言" line)
       {:token :序言 :text line})
     {:token :to-be-recognized :text line})))

(defn- 款-reducer [{ret :processed i :nth-款} [x & xs]]
  (letfn [(result [prevs {ty :token :as x}]
            {:processed (conj prevs x)
             :nth-款 (if (= ty :款) (:nth x) i)})

          (nth-款 [x i] (assoc x :token :款 :nth i))

          (some-higher-item? [xs target]
            (let [h (pt/doc-hierachy target)]
              (seq (filter #(let [hx (pt/doc-hierachy %)]
                              (and hx (> hx h)))
                           xs))))]
    (let [append-款 (comp (partial result ret) nth-款)
          to-be-recognized? (= (:token x) :to-be-recognized)
          {prev-ty :token prev-i :nth :as prev} (peek ret)]
      (cond
        (and to-be-recognized? (= prev-ty :条))
        (append-款 x 1)

        (and to-be-recognized? (= prev-ty :款))
        (append-款 x (inc prev-i))

        (and to-be-recognized? (= prev-ty :项))
        (let [[xsa [next-项 _]]
              (split-with #(not= (:token %) :项) xs)]
          (if (or (some-higher-item? xsa prev)
                  (not= (:nth next-项) (inc prev-i)))
            (append-款 x (inc i))
            (result (pop ret) (update prev :text str "\n" (:text x)))))

        :else (result ret x)))))

(defn- merge-款-across-lines
  {:test
   #(let [f merge-款-across-lines
          no-merge [[{:token :款 :nth 1 :text "结束。"}
                     {:token :款 :nth 2 :text "结束2。"}]
                    [{:token :款 :nth 1 :text "下列："}
                     {:token :款 :nth 2 :text "一段。"}
                     {:token :款 :nth 3 :text "二段。"}]
                    [{:token :款 :nth 1 :text "下列项："}]]]
      (tt/comprehend-tests
       (for [x no-merge]
         (t/is (= x (f x))))
       (t/is
        (= [{:token :款 :nth 1 :text "下列：\n一，\n二，\n三。"}
            {:token :款 :nth 2 :text "另起一款。"}
            {:token :款 :nth 3 :text "组成：\n你；\n我；\n它。"}
            {:token :款 :nth 4 :text "含义："}
            {:token :款 :nth 5 :text "某含义的解释。"}]
           (f [{:token :款 :nth 1 :text "下列："}
               {:token :款 :nth 2 :text "一，"}
               {:token :款 :nth 3 :text "二，"}
               {:token :款 :nth 4 :text "三。"}
               {:token :款 :nth 5 :text "另起一款。"}
               {:token :款 :nth 6 :text "组成："}
               {:token :款 :nth 7 :text "你；"}
               {:token :款 :nth 8 :text "我；"}
               {:token :款 :nth 9 :text "它。"}
               {:token :款 :nth 10 :text "含义："}
               {:token :款 :nth 11 :text "某含义的解释。"}])))))}
  [ks]
  (letfn [(ending-punct [k]
            (punct/symbol->label (last (:text k))))

          (list-head? [k]
            (= (ending-punct k) :colon))

          (list-item? [k]
            (punct/listing-seperators (ending-punct k)))

          (list-items [ks]
            (let [[ks ks'] (split-with list-item? ks)]
              (assert (seq ks') "undecided point reached; research!")
              [(concat ks (take 1 ks')) (rest ks')]))

          (merge-text [k k']
            (update k :text str "\n" (:text k')))

          (idx-dec-by [n ks]
            (map #(update % :nth - n) ks))]
    (if (<= (count ks) 1)
      ks ; no valid 款 listing
      (let [[k & ks'] ks]
        (assert (not (or (nil? k) (empty? ks'))))
        (if (not (list-head? k))
          (cons k (merge-款-across-lines ks')) ; pass k
          (let [[lis rest-ks] (list-items ks')]
            (if (<= (count lis) 1)
              ;; pass (into [k] lis)
              (concat (into [k] lis)
                      (merge-款-across-lines rest-ks))
              ;; read 款 listing and continue
              (cons (reduce merge-text k lis)
                    (->> rest-ks
                         (idx-dec-by (count lis))
                         merge-款-across-lines)))))))))

(defn draw-skeleton
  {:test
   #(let [f draw-skeleton
          lines ["总则"
                 "第一章 abc"
                 "第一条 cde"
                 "第三人fgh"
                 "（一）ijk"
                 "1.x"
                 "2.y"
                 "（二）lmn"
                 "……"
                 "（三）opq"
                 "结尾款"
                 "第二条 rst"
                 "分 则"
                 "第二章 ……"
                 "第二编 uvw"]
          r (f lines)]
      (tt/comprehend-tests
       (t/is (= {:token :则 :nth :general :text "总则"}  (nth r 0)))
       (t/is (= {:token :章 :nth 1 :text "第一章 abc"}   (nth r 1)))
       (t/is (= {:token :条 :nth 1 :text "第一条"}       (nth r 2)))
       (t/is (= {:token :款 :nth 1 :text "cde"}          (nth r 3)))
       (t/is (= {:token :款 :nth 2 :text "第三人fgh"}    (nth r 4)))
       (t/is (= {:token :项 :nth 1 :text "（一）ijk"}    (nth r 5)))
       (t/is (= {:token :目 :nth 1 :text "x"}            (nth r 6)))
       (t/is (= {:token :目 :nth 2 :text "y"}            (nth r 7)))
       (t/is (= {:token :项 :nth 2 :text "（二）lmn\n……"}(nth r 8)))
       (t/is (= {:token :项 :nth 3 :text "（三）opq"}    (nth r 9)))
       (t/is (= {:token :款 :nth 3 :text "结尾款"}       (nth r 10)))
       (t/is (= {:token :条 :nth 2 :text "第二条"}       (nth r 11)))
       (t/is (= {:token :款 :nth 1 :text "rst"}          (nth r 12)))
       (t/is (= {:token :则 :nth :special :text "分 则"} (nth r 13)))
       (t/is (= {:token :章 :nth 2 :text "第二章 ……"}    (nth r 14)))
       (t/is (= {:token :编 :nth 2 :text "第二编 uvw"}   (nth r 15)))))}
  [lines]
  (let [first-pass-ret (->> lines
                            (map prefixed-line-token)
                            flatten
                            (iterate rest)
                            (take-while seq)
                            (reduce 款-reducer {:processed [] :nth-款 0})
                            :processed)]
    (s/flatten-and-vector
     (s/map-on-binary-partitions
      #(= (:token %) :款) first-pass-ret
      merge-款-across-lines identity))))

(defn inject-contexts
  {:test
   #(let [f inject-contexts]
      (tt/comprehend-tests
       (t/is (empty? (f ())))
       (t/is (= [{:token :则 :nth :general :text "x"
                  :context {:则 :general}}]
                (f [{:token :则 :nth :general :text "x"}])))
       (let [s [{:token :to-be-recognized :text "a" :context {}}
                {:token :to-be-recognized :text "b" :context {}}]]
         (t/is (= s (f s))))
       (let [s [{:token :章 :nth 3 :text "第三章 法 人"}
                {:token :节 :nth 1 :text "第一节 一般规定"}
                {:token :条 :nth 36 :text "第三十六条"}
                {:token :款 :nth 1 :text "法人是具有民事权利……"}
                {:token :款 :nth 2 :text "法人的民事权利能力……"}
                {:token :条 :nth 37 :text "第三十七条"}
                {:token :款 :nth 1 :text "法人应当具备下列条件："}
                {:token :项 :nth 1 :text "（一）依法成立；"}]]
         (t/is (= [{:token :章 :nth 3 :text "第三章 法 人"
                    :context {:章 3}}
                   {:token :节 :nth 1 :text "第一节 一般规定"
                    :context {:章 3 :节 1}}
                   {:token :条 :nth 36 :text "第三十六条"
                    :context {:章 3 :节 1 :条 36}}
                   {:token :款 :nth 1 :text "法人是具有民事权利……"
                    :context {:章 3 :节 1 :条 36 :款 1}}
                   {:token :款 :nth 2 :text "法人的民事权利能力……"
                    :context {:章 3 :节 1 :条 36 :款 2}}
                   {:token :条 :nth 37 :text "第三十七条"
                    :context {:章 3 :节 1 :条 37 :款 2}}
                   {:token :款 :nth 1 :text "法人应当具备下列条件："
                    :context {:章 3 :节 1 :条 37 :款 1}}
                   {:token :项 :nth 1 :text "（一）依法成立；"
                    :context {:章 3 :节 1 :条 37 :款 1 :项 1}}]
                  (f s))))))}
  [tls]
  (reduce
   (fn [ret {t :token :as x}]
     (let [curr (:context (peek ret))
           succ (cond-> curr
                  (not= t :to-be-recognized) (assoc t (:nth x)))]
       (conj ret (cond-> x
                   succ (assoc :context succ)))))
   [] tls))
