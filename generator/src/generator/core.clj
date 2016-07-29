(ns generator.core
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :as t]
            [generator.test :as tt]
            [generator.tokenizer :as token]
            [hiccup.core :refer :all]
            [hiccup.page :refer :all])
  (:gen-class))

(defn- without-prefix [origin prefix]
  (loop [s origin t prefix]
    (cond
      (empty? t) s
      (not= (first s) (first t)) origin
      :else (recur (rest s) (rest t)))))

(defn default-fn [l] [:p l])

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
  (let [head (first ls)]
    (assert (re-matches table-of-contents-sentinel head))
    (if-let [first-item (second ls)]
      (loop [s (rest (rest ls))
             t [head first-item]]
        (if (or (= (first s) first-item) (empty? s))
          [t {:token :table-of-contents :list  t}]
          (recur (rest s) (conj t (first s)))))
      [[head] {:token :table-of-contents :list [head]}])))

(defn use-chinese-paren
  {:test
   #(let [f use-chinese-paren]
      (tt/comprehend-tests
       [(t/is (= "（）"     (f "()")))
        (t/is (= "（六）"   (f "(六)")))
        (t/is (= "六）"     (f "六)")))
        (t/is (= "六）（七" (f "六)（七")))]))}
  [s]
  (str/replace s #"[\(\)]" #(case %1
                              "(" "（"
                              ")" "）")))

(defn space-clapsed
  {:test
   #(let [f space-clapsed]
      (tt/comprehend-tests
       (t/is (= "az" (f "az")))
       (t/is (= "a z" (f "a z")))
       (t/is (= "a z" (f "a  z")))))}
  [s]
  (str/join " " (str/split s #"\s+")))

(defn space-filled
  {:test
   #(let [f space-filled]
      (tt/comprehend-tests
       (t/is (= "az" (f "az")))
       (t/is (= "a-z" (f "a z")))
       (t/is (= "a-z-a" (f "a z a")))))}
  [s]
  (str/replace s #"\s" "-"))

(defn- draw-skeleton [lines]
  (loop [ls lines
         es []]
    (cond
      (empty? ls)
      (seq es)

      (re-matches table-of-contents-sentinel (first ls))
      (let [[processed recognized] (table-of-contents ls)]
        (recur (without-prefix ls processed)
               (conj es recognized)))

      (let [[_ unit] (token/nth-item (first ls))]
        (#{\章 \节} unit))
      (let [line (first ls)]
        (recur (rest ls) (conj es (token/nth-章节 ls))))

      (= (second (token/nth-item (first ls))) \条)
      (let [[processed recognized] (token/nth-条 ls)]
        (recur (without-prefix ls processed)
               (conj es recognized)))

      :else
      (recur (rest ls) (conj es {:token :to-be-recognized :text (first ls)})))))

(defn within-款项
  {:test
   #(let [f within-款项
          a  (str "（三）连续订立二次固定期限劳动合同，且劳动者没有"
                  "本法第三十九条和第四十条第一项、第二项规定的情形，"
                  "续订劳动合同的。")
          sa (seq a)
          b  (str "劳动者有下列情形之一的，用人单位不得依照"
                  "本法第四十条、第四十一条的规定解除劳动合同：")
          sb (seq b)]
      (tt/comprehend-tests
       (t/is (= a (str/join (map :text (f {} sa)))))
       (t/is (= b (str/join (map :text (f {} sb)))))))}
  [context cs]
  (let [flags [(partial = \本) #{\法 \条} (partial = \第) token/numchar-zh-set]

        check-flags
        (fn [[a b c d]]
          (and ((flags 0) a) ((flags 1) b) ((flags 2) c) ((flags 3) d)))

        generate-id (partial token/generate-id context)]
    (->> (loop [cs cs ts []]
           (if (empty? cs)
             ts
             (if (check-flags cs)
               (let [[items rests] (token/read-items cs)]
                 (recur rests (into ts (flatten (token/update-leaves
                                                 (token/parse items)
                                                 :id generate-id)))))
               (recur (rest cs) (conj ts {:token :to-be-recognized
                                          :text (str (first cs))}))))))))

(defn within-条
  {:test
   #(let [f within-条
          a "第一条"
          b " 第一款内容"
          c "第二款内容"
          d "（一）第一项内容"
          e "第三款内容"
          r (f [(str a b) c d e])]
      (tt/comprehend-tests
       (t/is (= [:条头 \款 \款 \项 \款] (map :token r)))
       (t/is (= [1 1 2 1 3] (map :nth r)))
       (t/is (= [a (str/trim b) c d e] (map :text r)))))}
  [[line & lines]]
  (when-let [head (token/条头 (cons \newline line))]
    (let [tail     (without-prefix line (:text head))
          first-款 (str/join (rest tail))] ;use "rest" to skip \space
      (assert (seq first-款))
      (loop [ts [head {:token \款 :nth 1 :text first-款}]
             [l & ls] lines
             i-款 2]
        (if (nil? l)
          ts
          (if (= (first l) \（)
            (if-let [t (token/nth-项 l)]
              (recur (conj ts t) ls i-款)
              (recur (conj ts {:token :to-be-recognized :text l}) ls i-款))
            (recur (conj ts {:token \款 :nth i-款 :text l}) ls (inc i-款))))))))

(defn wrap-item-string-in-html
  {:test
   #(let [f wrap-item-string-in-html
          ts [{:token \法, :nth :this, :text "本法"}
              {:token \条, :nth 39, :text "第三十九条", :id "条39"}
              {:token :separator, :text "和"}
              {:token \条, :nth 40, :text "第四十条"}
              {:token \款, :nth 1}
              {:token \项, :nth 1, :text "第一项", :id "条40款1项1"}
              {:token :separator, :text "、"}
              {:token \项, :nth 2, :text "第二项", :id "条40款1项2"}]]
      (tt/comprehend-tests
       (t/is (= (html [:span
                       "本法"
                       [:a {:href "#条39"} "第三十九条"]
                       "和第四十条"
                       [:a {:href "#条40款1项1"} "第一项"]
                       "、"
                       [:a {:href "#条40款1项2"} "第二项"]])
                (html (f ts))))))}
  [ts]
  [:span (let [ts' (->> ts
                        (partition-by :id)
                        (map #(if (:id (first %))
                                %
                                {:text (str/join (map :text %))}))
                        flatten)]
           (for [t ts'
                 :let [{:keys [id text]} t]]
             (if id
               [:a {:href (str \# id)} text]
               text)))])

(defn- wrap-条-in-html [[head & more-tokens]]
  (assert (= (:token head) :条头))
  [:div {:class "entry" :id (str \条 (:nth head))}
   [:div {:class "title"}
    [:b {:class "title"} (:text head)]]
   (seq
    (loop [ps []
           [t & ts] more-tokens
           i-款 0]
      (if (nil? t)
        ps
        (let [content (->> (within-款项 {\条 (:nth head)} (seq (:text t)))
                           (partition-by #(= (:token %) :to-be-recognized))
                           (map #(if (= (:token (first %)) :to-be-recognized)
                                   (str/join (map :text %))
                                   (wrap-item-string-in-html %))))]
          (condp = (:token t)
            \款 (recur (conj ps [:p {:class "款"
                                     :id (str "条" (:nth head)
                                              "款" (inc i-款))}
                                 content])
                       ts
                       (inc i-款))
            \项 (recur (conj ps [:p {:class "项"
                                     :id (str "条" (:nth head)
                                              "款" i-款
                                              "项" (:nth t))}
                                 content])
                       ts
                       i-款))))))])

(defn- wrap-in-html [tokenized-lines]
  (html
   (html5
    [:head {:lang "zh"}
     [:meta {:charset "utf-8"}]
     [:meta {:name "viewport"
             :content "width=device-width, initial-scale=1"}]
     [:title (:text (first tokenized-lines))]
     [:link {:rel "stylesheet" :href "index.css"}]
     [:script {:src "main.js"}]]
    [:body
     [:div {:id "entries-container"}
      (for [{t :token :as tl} tokenized-lines]
        (case t
          :table-of-contents
          (let [[head & item-list] (:list tl)]
            [:nav [:h2 head]
             [:ul (for [item item-list]
                    [:li [:a {:href (str "#" (space-filled item))}
                          item]])]])

          \章
          (let [txt (:text tl)]
            [:h2 {:id (space-filled txt) :class "章"} txt])

          \节
          (let [txt (:text tl)]
            [:h3 {:id (space-filled txt) :class "节"} txt])

          \条
          (wrap-条-in-html (within-条 (:text tl)))

          :to-be-recognized
          (default-fn (:text tl))))]])))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (with-open [r (io/reader "../original_text.txt")]
    (->> (line-seq r)
         (remove str/blank?)
         (map (comp use-chinese-paren space-clapsed str/trim))
         draw-skeleton
         wrap-in-html
         (spit "../index.html"))))

(-main)
