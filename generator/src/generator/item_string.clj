(ns generator.item-string
  (:require [clojure.set :as set]
            [clojure.string :as str]
            [clojure.test :as t]
            [generator.line :as l]
            [generator.lisp :as s]
            [generator.test :as tt]
            [generator.parse-tree :as pt]
            [generator.zh-digits :refer [数字 numchar-zh-set]]))

(def from-第 (partial s/from-x \第))
(def to-条 (partial s/to-x \条))

(def examples
  ["本法第三十九条和第四十条第一项、第二项"
   "本法第三十九条和第四十条第一项、第二项"
   "本法第二十二条和第二十三条"
   "本法第二十六条第一款"
   "本法第二十六条第一款第一项"
   "本条第一款"
   "本法第四十条、第四十一条"
   "本法第四十二条"
   "本法第四十二条第二项"
   "本法第三十八条"
   "本法第三十六条"
   "本法第四十条"
   "本法第四十一条第一款"
   "本法第四十四条第一项"
   "本法第四十四条第四项、第五项"
   "本法第八十七条"
   "本法第十七条"
   "本法第三十六条、第三十八条"
   "本法第三十九条和第四十条第一项、第二项"
   "本法第二十六条"
   "本法第四十七条"
   "本法第十四条 第二款第三项"
   "本法第四十六条"
   ;;"前款第（五）、第（六）项"
   "本规定第十、十八、二十六、二十七条"
   "本办法第五条第一款"
   ])

(defn- separators
  {:test
   #(let [f separators]
      (tt/comprehend-tests
       (t/is (nil? (f "第一条")))
       (t/is (= [{:token :separator :text "或者"}
                 (seq "第三款")]
                (f "或者第三款")))))}
  [cs]
  (let [choices #{" " "、" "和" "或" "或者" "至"}

        longest-match
        (fn [ss]
          (when (seq ss)
            (apply max-key count ss)))

        result
        (longest-match
         (remove nil?
                 (for [cc choices
                       :let [csn (take (count cc) cs)]]
                   (when (= cc (str/join csn))
                     cc))))]
    (when result
      [{:token :separator :text result}
       (s/without-prefix cs (seq result))])))

(def item-types-1 #{:编 :章 :节 :条 :款 :项})
(def item-types-2 #{:法 :规定 :办法 :条例})
(def item-types (set/union item-types-1 item-types-2))

(def ^:private cs-kw (comp keyword str/join))
(def item-type-str name)

(defn nth-items
  {:test
   #(let [f nth-items
          invalid-inputs ["第一" "第二、第三" "第四、第五abc"]]
      (tt/comprehend-tests
       (for [in (map seq invalid-inputs)]
         (t/is (nil? (f in))))
       (t/is (= [[{:nth 1 :token :条 :第? true :unit? true :text "一"}]
                 []]
                (f (seq "第一条"))))
       (t/is (= [[{:nth 2 :token :款 :第? true :unit? false :text "二"}
                  {:token :separator :text "、"}
                  {:nth 3 :token :款 :第? true :unit? true :text "三"}]
                 []]
                (f (seq "第二、第三款"))))
       (t/is (= [[{:nth 4 :token :项 :第? true :unit? false :text "四"}
                  {:token :separator :text "、"}
                  {:nth 5 :token :项 :第? false :unit? false :text "五"}
                  {:token :separator :text "、"}
                  {:nth 6 :token :项 :第? false :unit? true :text "六"}]
                 (seq "规定的")]
                (f (seq "第四、五、六项规定的"))))
       (t/is (= [[{:nth 5 :token :项 :第? true :unit? false :text "（五）"}
                  {:token :separator :text "、"}
                  {:nth 6 :token :项 :第? true :unit? true :text "（六）"}]
                 (seq "有关")]
                (f (seq "第（五）、第（六）项有关"))))))}
  [char-seq]
  (assert (= (first char-seq) \第))
  (letfn [(passively-assign-token [ts]
            (when-let [it (:token (peek ts))]
              (map (fn [t]
                     (if (:token t)
                       t
                       (assoc t :token it))) ts)))]
    (loop [cs char-seq
           ts []]
      (if (:unit? (peek ts))
        [(passively-assign-token ts) cs]
        (let [c       (first cs)
              第?     (= c \第)
              cs      (cond 第? (rest cs)
                            (numchar-zh-set c) cs)
              [n ncs] (or (l/括号数字 cs) (数字 cs))
              cs'     (s/without-prefix cs ncs)
              nx      {:nth n :text (str/join ncs) :第? 第?}]
          (when-let [c' (first cs')]
            (if (item-types-1 (cs-kw [c']))
              (recur (rest cs')
                     (conj ts (assoc nx
                                     :unit? true
                                     :token (cs-kw [c']))))
              (when-let [[s-t rest-cs'] (separators cs')]
                (recur rest-cs'
                       (into ts [(assoc nx :unit? false) s-t]))))))))))

(defn- match-item-types [cs]
  (let [item-type-css (map (comp seq item-type-str) item-types)]
    (first (remove nil? (for [tycs item-type-css
                              :let [n (count tycs)]]
                          (when (= (take n cs) tycs)
                            tycs))))))

(def adj->nth {\本 :this \前 :prev})

(defn read-items
  {:test
   #(let [f read-items]
      (tt/comprehend-tests
       (t/is (= [[{:token :法 :nth :this :text "本法"}
                  {:token :条 :nth 20 :text "二十" :第? true :unit? true}
                  {:token :项 :nth 3 :text "三" :第? true :unit? true}]
                 [\规 \定]]
                (f "本法第二十条第三项规定")))
       (t/is (= [[{:token :规定 :nth :this :text "本规定"}
                  {:token :条 :nth 10 :第? true :unit? false :text "十"}
                  {:token :separator :text "、"}
                  {:token :条 :nth 18 :第? false :unit? false :text "十八"}
                  {:token :separator :text "、"}
                  {:token :条 :nth 26 :第? false :unit? false :text "二十六"}
                  {:token :separator :text "、"}
                  {:token :条 :nth 27 :第? false :unit? true :text "二十七"}]
                 [\有 \关 \规 \定]]
                (f "本规定第十、十八、二十六、二十七条有关规定")))))}
  [char-seq]
  (loop [[c & more-cs :as cs] char-seq
         ts []]
    (if-let [[s-t cs'] (separators cs)]
      (recur cs' (conj ts s-t))
      (if-let [adj (adj->nth c)]
        (when-let [processed (match-item-types more-cs)]
          (recur (s/without-prefix more-cs processed)
                 (let [processed-str (str/join processed)]
                   (conj ts {:token (keyword processed-str)
                             :nth adj
                             :text (str c processed-str)}))))
        (case c
          \第 (when-let [[ts' rest-cs] (nth-items cs)]
                (recur rest-cs (into ts ts')))
          [ts cs])))))

(defn parse [recognized-items]
  (pt/linear-to-tree recognized-items
                     pt/doc-hierachy
                     [pt/款-filler]))

(defn second-pass
  {:test
   #(let [f second-pass]
      (tt/comprehend-tests
       (t/is (= [{:token :法 :nth :this :text "本法"}
                 {:token :条 :nth 39 :text "第三十九条" :第? true :unit? true :id "条39"}
                 {:token :separator :text "和"}
                 {:token :条 :nth 40 :text "第四十条" :第? true :unit? true}
                 {:token :款 :nth 1}
                 {:token :项 :nth 1 :text "第一项" :第? true :unit? true :id "条40款1项1"}
                 {:token :separator :text "、"}
                 {:token :项 :nth 2 :text "第二项" :第? true :unit? true :id "条40款1项2"}]
                (f [{:token :法 :nth :this :text "本法"}
                    {:token :条 :nth 39 :text "三十九" :第? true :unit? true :id "条39"}
                    {:token :separator :text "和"}
                    {:token :条 :nth 40 :text "四十" :第? true :unit? true}
                    {:token :款 :nth 1}
                    {:token :项 :nth 1 :text "一" :第? true :unit? true :id "条40款1项1"}
                    {:token :separator :text "、"}
                    {:token :项 :nth 2 :text "二" :第? true :unit? true :id "条40款1项2"}])))
       (t/is (= [{:token :规定 :nth :this :text "本规定"}
                 {:text "第"}
                 {:token :条 :nth 10 :text "十" :第? true :unit? false :id "条10"}
                 {:token :separator :text "、"}
                 {:token :条 :nth 18 :text "十八" :第? false :unit? false :id "条18"}
                 {:token :separator :text "、"}
                 {:token :条 :nth 26 :text "二十六" :第? false :unit? false :id "条26"}
                 {:token :separator :text "、"}
                 {:token :条 :nth 27 :text "二十七" :第? false :unit? true :id "条27"}
                 {:text "条"}]
                (f [{:token :规定 :nth :this :text "本规定"}
                    {:token :条 :nth 10 :text "十" :第? true :unit? false :id "条10"}
                    {:token :separator :text "、"}
                    {:token :条 :nth 18 :text "十八" :第? false :unit? false :id "条18"}
                    {:token :separator :text "、"}
                    {:token :条 :nth 26 :text "二十六" :第? false :unit? false :id "条26"}
                    {:token :separator :text "、"}
                    {:token :条 :nth 27 :text "二十七" :第? false :unit? true :id "条27"}])))
       (t/is (= [{:token :款 :nth :prev :text "前款"}
                 {:token :项 :nth 5 :text "第（五）" :第? true :unit? false}
                 {:token :separator :text "、"}
                 {:token :项 :nth 6 :text "第（六）" :第? true :unit? true}
                 {:text "项"}]
                (f [{:token :款 :nth :prev :text "前款"}
                    {:token :项 :nth 5 :text "（五）" :第? true :unit? false}
                    {:token :separator :text "、"}
                    {:token :项 :nth 6 :text "（六）" :第? true :unit? true}])))))}
  [ts]
  (letfn [(prev-item [ts]
            (if (empty? ts)
              nil
              (let [t (peek ts)]
                (if (item-types (:token t))
                  t
                  (recur (pop ts))))))

          (succ-item [[suc ssuc]]
            (cond (item-types (:token suc)) suc
                  (item-types (:token ssuc)) ssuc))

          (extend-to [{ty :token :as t} left? right?]
            (cond-> t
              (and left? (:第? t)) (update :text (partial str "第"))
              (and right? (:unit? t)) (update :text str (item-type-str ty))))

          (merge-both [t]
            [(extend-to t true true)])

          (merge-left [{ty :token :as t}] (assert (:unit? t))
            [(extend-to t true false) {:text (item-type-str ty)}])

          (merge-right [t] (assert (:第? t))
            [{:text "第"} (extend-to t false true)])]
    (reduce
     (fn [ts' [{ty :token :as t} tsuc tssuc]]
       (assert t)
       (cond (= ty :separator)
             (conj ts' t)

             (item-types ty)
             (into ts'
                   (if-not (or (:第? t) (:unit? t))
                     [t]
                     (let [pre (prev-item ts')
                           suc (succ-item [tsuc tssuc])]
                       (cond
                         ;;in the middle of a group
                         (= (:token pre)
                            ty
                            (:token suc)) (merge-both t)
                         ;;at the beginning of a group
                         (= ty
                            (:token suc)) (if (:第? suc)
                                            (merge-both t)
                                            (merge-right t))
                         ;;at the end of a group
                         (= (:token pre)
                            ty)           (if (:unit? pre)
                                            (merge-both t)
                                            (merge-left t))
                         :standalone      (merge-both t)))))))
     [] (partition 3 1 (concat ts [nil nil])))))

(defn str-token
  {:test
   #(let [f str-token
          [a b c] (first (nth-items "第（二）、第（三）项"))]
      (tt/comprehend-tests
       (t/is (= "第一条" (f (first (first (nth-items "第一条"))))))
       (t/is (= "第（二）"   (f a)))
       (t/is (= "、"     (f b)))
       (t/is (= "第（三）项"   (f c)))))}
  [{ty :token :as t}]
  (if (item-types ty)
    (str (if (:第? t) \第 "")
         (:text t)
         (if (:unit? t) (item-type-str ty) ""))
    (:text t)))

(defn 条头
  {:test
   #(let [f 条头]
      (tt/comprehend-tests
       (t/is (not (f "第三十九条")))
       (t/is (not (f "\n第三十九")))
       (t/is (not (f "\n第三十九 条")))
       (t/is (not (f "\n三十九条")))
       (t/is (not (f "\n第39条")))
       (t/is (= {:token :条头 :nth 39 :text "第三十九条"}
                (f "\n第三十九条")))))}
  [[c & cs]]
  (and (= c \newline)
       (when-let [[begin body end] (s/read-xs from-第 to-条 cs)]
         (let [[i processed] (数字 body)]
           (and (= processed body)
                {:token :条头 :nth i
                 :text (str/join
                        (s/flatten-and-vector begin body end))})))))
