(ns generator.tokenizer
  (:require [clojure.string :as str]
            [clojure.test :as t]
            [clojure.zip :as z]
            [generator.test :as tt]))

(defn without-prefix [origin prefix]
  (loop [s origin t prefix]
    (cond
      (empty? t) s
      (not= (first s) (first t)) origin
      :else (recur (rest s) (rest t)))))

(def cdigit-map
  {\零 0 \一 1 \二 2 \三 3 \四 4 \五 5 \六 6 \七 7 \八 8 \九 9})

(def nthten-map
  {\十 10 \百 100})

(def numchar-zh-set (set (concat (keys cdigit-map) (keys nthten-map))))

(defn 数字
  {:test
   #(let [f 数字]
      (tt/comprehend-tests
       (t/is (= [0 []]                      (f "")))
       (t/is (= [0 [\零]]                   (f "零")))
       (t/is (= [1 [\一]]                   (f "一")))
       (t/is (= [10 [\十]]                  (f "十")))
       (t/is (= [12 [\十 \二]]              (f "十二章")))
       (t/is (= [20 [\二 \十]]              (f "二十行")))
       (t/is (= [34 [\三 \十 \四]]          (f "三十四")))
       (t/is (= [567 [\五 \百 \六 \十 \七]] (f "五百六十七")))
       (t/is (= [809 [\八 \百 \零 \九]]     (f "八百零九回")))))}
  [s]
  (let [c (first s)]
    (cond (empty? s) [0 []]
          (= c \十)  (let [[x s'] (数字 (rest s))]
                       [(+ 10 x) (cons \十 s')])
          (= c \零)  (let [[x s'] (数字 (rest s))]
                       [x (cons \零 s')])

          (contains? cdigit-map c)
          (let [d (cdigit-map c)
                n (second s)]
            (if (contains? nthten-map n)
              (let [[x s'] (数字 (nthrest s 2))]
                [(+ (* d (nthten-map n)) x) (into [c n] s')])
              [d [c]]))

          :else [0 []])))

(defn nth-item
  {:test
   #(let [f nth-item]
      (tt/comprehend-tests
       (t/is (= [1 \章] (f "第一章 总则")))
       (t/is (= [12 \条] (f "第十二条 ……")))))}
  [line]
  (let [[c & cs] line]
    (when (= c \第)
      (let [[i processed] (数字 cs)]
        [i (first (without-prefix cs processed))]))))

(defn nth-章节
  {:test
   #(let [f nth-章节]
      (tt/comprehend-tests
       (t/is (= {:token \章 :nth 1 :text "第一章 总则"}
                (f ["第一章 总则" "第一条 ……"])))
       (t/is (= {:token \节 :nth 2 :text "第二节 劳务派遣"}
                (f ["第二节 劳务派遣" "第五十七条 ……"])))))}
  [lines]
  (let [line (first lines)
        [i unit] (nth-item line)]
    (assert (and i (#{\章 \节} unit)))
    {:token unit :nth i :text line}))

(defn nth-条
  {:test
   #(let [f nth-条
          a ["第五条 县级以上……"]
          b ["第六条 工会应当……" "……"]
          c ["第二章 ……"]
          d ["第七条 用人单位……"]
          ab (into a b)
          bcd (-> b (into c) (into d))]
      (tt/comprehend-tests
       (t/is (= [a {:token \条 :nth 5 :text a :head "第五条"}]
                (f ab)))
       (t/is (= [b {:token \条 :nth 6 :text b :head "第六条"}]
                (f bcd)))))}
  [lines]
  (let [line (first lines)
        [i unit] (nth-item line)]
    (assert (and i (= unit \条)))
    (let [lines-within
          (into [line] (take-while
                        (fn [l]
                          (let [[_ unit] (nth-item l)]
                            (nil? (#{\章 \节 \条} unit))))
                        (rest lines)))]
      [lines-within
       {:token \条 :nth i :head (first (str/split line #"\s"))
        :text lines-within}])))

(def txts ["本法第三十九条和第四十条第一项、第二项"
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
           ])

;(clojure.pprint/pprint (map #(str/split % #"[法条款项和、]") txts))

(declare 括号数字)
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
  (let [item-types #{\条 \款 \项}

        passively-assign-token
        (fn [ts]
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
              [n ncs] (or (括号数字 cs) (数字 cs))
              cs'     (without-prefix cs ncs)
              nx      {:nth n :text (str/join ncs) :第? 第?}]
          (when-let [c' (first cs')]
            (if (item-types c')
              (recur (rest cs')
                     (conj ts (assoc nx :unit? true :token (keyword (str c')))))
              (when-let [s-t (separators c')]
                (recur (rest cs')
                       (into ts [(assoc nx :unit? false) s-t]))))))))))

(defn- separators [c]
  (when (#{\space \、 \和} c)
    {:token :separator :text (str c)}))

(def item-types #{[\法] [\规 \定] [\条] [\款] [\项]})

(defn match-item-types [cs]
  (first (remove nil? (for [target item-types
                            :let [n (count target)]]
                        (when (= (take n cs) target)
                          target)))))

(defn item-type-token [i cs]
  (assert (item-types cs))
  (let [s (str/join cs)]
    {:token (keyword s) :nth i :text s}))

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
  (let [adj->nth {\本 :this}]
   (loop [[c & more-cs :as cs] char-seq
          ts []]
     (if-let [s-t (separators c)]
       (recur more-cs (conj ts s-t))
       (if-let [adj (adj->nth c)]
         (when-let [processed (match-item-types more-cs)]
           (recur (without-prefix more-cs processed)
                  (let [processed-str (str/join processed)]
                    (conj ts {:token (keyword processed-str)
                              :nth adj
                              :text (str c processed-str)}))))
         (case c
           \第 (when-let [[ts' rest-cs] (nth-items cs)]
                 (recur rest-cs (into ts ts')))
           [ts cs]))))))

(def parse-tree (partial z/zipper
                         seq? ;branch?
                         rest ;children
                         (fn [x children] ;makenode
                           (cons (first x) children))))

(defn parse [recognized-items]
  (z/root
   (reduce
    (fn [loc x]
      (let [curr-t (:token (if (z/branch? loc)
                             (first (z/node loc))
                             (z/node loc)))
            x-t (:token x)]
        (case [curr-t x-t]
          [:法 :条] (-> loc (z/append-child (list x)) z/down z/rightmost)
          [:规定 :条] (-> loc (z/append-child (list x)) z/down z/rightmost)
          [:条 :款] (-> loc (z/append-child (list x)) z/down z/rightmost)
          [:条 :项] (-> loc (z/append-child (list {:token :款 :nth 1})) z/down z/rightmost
                        (z/append-child (list x)) z/down z/rightmost)
          [:款 :项] (-> loc (z/append-child (list x)) z/down z/rightmost)

          [:条 :条] (recur (-> loc z/up) x)
          [:款 :款] (recur (-> loc z/up) x)
          [:项 :项] (recur (-> loc z/up) x)

          [:款 :条] (recur (-> loc z/up z/up) x)
          [:项 :款] (recur (-> loc z/up z/up) x)
          [:项 :条] (recur (-> loc z/up z/up z/up) x)

          (if (= x-t :separator)
            (-> loc (z/append-child x))
            (do (println "unrecognized pattern" [curr-t x])
                loc)))))
    (parse-tree (list (first recognized-items)))
    (rest recognized-items))))

(defn generate-id
  {:test
   #(let [f generate-id]
      (tt/comprehend-tests
       (t/is (= ""          (f {} [])))
       (t/is (= "条1"       (f {} [{:token :法 :nth :this}
                                   {:token :条 :nth 1}])))
       (t/is (= "条1"       (f {:条 1} [{:token :条 :nth :this}])))
       (t/is (= "条1款2项3" (f {} [{:token :法 :nth :this}
                                   {:token :条 :nth 1}
                                   {:token :款 :nth 2}
                                   {:token :项 :nth 3}])))))}
  [context src]
  (str/join
   (loop [r () s src]
     (cond (empty? s)
           r

           (= (count s) 1)
           (let [c-t (:token (peek s))]
             (assert (= (:nth (peek s)) :this))
             (cond-> r
               (nil? (#{:法 :规定} c-t)) (into [(context c-t) (name c-t)])))

           :else
           (let [c (peek s)]
             (recur (into r [(:nth c) (name (:token c))]) (pop s)))))))

(defn str-token
  {:test
   #(let [f str-token
          [a b c] (first (nth-items "第（二）、第（三）项"))]
      (tt/comprehend-tests
       (t/is (= "第一条" (f (first (first (nth-items "第一条"))))))
       (t/is (= "第（二）"   (f a)))
       (t/is (= "、"     (f b)))
       (t/is (= "第（三）项"   (f c)))))}
  [t]
  (let [item-tokens (set (map (comp keyword str/join) item-types))
        token (:token t)]
    (if (item-tokens token)
      (str (if (:第? t) \第 "")
           (:text t)
           (if (:unit? t) (name token) ""))
      (:text t))))

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

(let [items (comp first read-items)]
  (tt/comprehend-tests
   (for [src txts
         :let [r (parse (items src))]]
     (do
       ;; (clojure.pprint/pprint r)
       ;; (println "------------------------")
       (t/is (= src (str/join (map str-token (flatten r)))))))
   (let [r (parse (items (seq "本法第三十九条和第四十条第一项、第二项")))]
     (t/is
      (= '({:token :法 :nth :this :text "本法"}
           ({:token :条 :nth 39 :text "三十九" :第? true :unit? true :id "条39"}
            {:token :separator :text "和"})
           ({:token :条 :nth 40 :text "四十" :第? true :unit? true}
            ({:token :款 :nth 1}
             ({:token :项 :nth 1 :text "一" :第? true :unit? true :id "条40款1项1"}
              {:token :separator :text "、"})
             ({:token :项 :nth 2 :text "二" :第? true :unit? true :id "条40款1项2"}))))
         (update-leaves r :id (partial generate-id {})))))
   (let [r (parse (items (seq "本规定第十、十八、二十六、二十七条")))]
     (t/is
      (= '({:token :规定 :nth :this :text "本规定"}
           ({:token :条 :nth 10 :text "十" :第? true :unit? false :id "条10"}
            {:token :separator :text "、"})
           ({:token :条 :nth 18 :text "十八" :第? false :unit? false :id "条18"}
            {:token :separator :text "、"})
           ({:token :条 :nth 26 :text "二十六" :第? false :unit? false :id "条26"}
            {:token :separator :text "、"})
           ({:token :条 :nth 27 :text "二十七" :第? false :unit? true :id "条27"}))
         (update-leaves r :id (partial generate-id {})))))))

(defn- read-chars [from to cs]
  (when-let [[begin body] (from cs)]
    (when-let [[body end] (to body)]
      [begin body end])))

(defn from-char
  {:test
   #(let [f (partial from-char \第)]
      (tt/comprehend-tests
       (t/is (nil? (f [])))
       (t/is (nil? (f [\newline \第])))
       (t/is (= [[\第] [\a \b \c]] (f [\第 \a \b \c])))))}
  [beginc [c & cs]]
  (when (= c beginc) [[c] cs]))

(defn to-char
  {:test
   #(let [f (partial to-char \条)]
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

(def from-第 (partial from-char \第))
(def to-条 (partial to-char \条))

(defn into-str
  {:test
   #(let [f into-str]
      (tt/comprehend-tests
       (t/is (= ""     (into-str)))
       (t/is (= "abc"  (into-str [\a \b \c])))
       (t/is (= "abcd" (into-str [\a] [\b] [\c \d])))))}
  ([] "")
  ([s] (str/join s))
  ([s & ss] (str/join (reduce into (vec s) ss))))

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
       (when-let [[begin body end] (read-chars from-第 to-条 cs)]
         (let [[i processed] (数字 body)]
           (and (= processed body)
                {:token :条头 :nth i
                 :text (into-str begin body end)})))))

(defn 括号数字
  {:test
   #(let [f 括号数字]
      (tt/comprehend-tests
       (t/is (not (f "（")))
       (t/is (not (f "（）")))
       (t/is (not (f "（括号内）")))
       (t/is (= [1 [\（ \一 \）]] (f "（一）……")))))}
  [cs]
  (let [openfn (partial from-char \（)
        closefn (partial to-char \）)]
    (when-let [[begin body end] (read-chars openfn closefn cs)]
      (let [[i processed] (数字 body)]
        (when (and (seq processed) (= processed body))
          [i (reduce into begin [body end])])))))

(defn nth-项
  {:test
   #(let [f nth-项]
      (tt/comprehend-tests
       (t/is (= nil (f "（")))
       (t/is (= nil (f "（括号内")))
       (t/is (= {:token \项 :nth 5 :head "（五）" :text "（五）……"}
                (f "（五）……")))))}
  [line]
  (when-let [[i processed] (括号数字 line)]
    {:token \项 :nth i :head (str/join processed) :text line}))

(defn seq-match
  {:test
   #(let [f (partial seq-match
                     [#{[\本]}
                      #{[\规 \定] [\法] [\条]}
                      #{[\第]}
                      (set (map vector numchar-zh-set))])]
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
      (if-let [word (some seq (for [word word-set]
                                (when (= word (take (count word) cs))
                                  word)))]
        [true (without-prefix cs word)]
        (reduced [false cs])))
    [true cs] word-sets)))
