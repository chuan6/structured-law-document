(ns generator.core
  (:require [clj-http.util :as http]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :as t]
            [generator.id :as id]
            [generator.line :as ln]
            [generator.lisp :as s]
            [generator.test :as tt]
            [generator.tokenizer :as tk]
            [generator.parse-tree :as pt]
            [generator.zh-digits :refer [numchar-zh-set]]
            [hiccup.core :refer :all]
            [hiccup.page :refer :all])
  (:gen-class))

(defn default-fn [l] [:p l])

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
       (t/is (= "a z" (f "a  z")))
       (t/is (= "a z" (f "a　z"))) ;U+3000 - ideographic space
       ))}
  [s]
  (str/join " " (str/split s #"[\s\u3000]+")))

(defn space-filled
  {:test
   #(let [f space-filled]
      (tt/comprehend-tests
       (t/is (= "az" (f "az")))
       (t/is (= "a-z" (f "a z")))
       (t/is (= "a-z-a" (f "a z a")))))}
  [s]
  (str/replace s #"\s" "-"))

(defn- 条-rise [txs]
  (pt/linear-to-tree txs pt/doc-hierachy))

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
  (let [genid (partial id/generate context)
        flags [#{[\本] [\前]}
               #{[\办 \法] [\规 \定] [\法] [\条] [\款]}
               #{[\第]}]]
    (loop [cs cs ts []]
      (if (empty? cs)
        ts
        (if (s/seq-match flags cs)
          (let [[items rests] (tk/read-items cs)]
            (recur rests (into ts (-> items
                                      tk/parse
                                      (pt/update-leaves :id genid)
                                      flatten
                                      tk/second-pass))))
          (recur (rest cs) (conj ts {:token :to-be-recognized
                                     :text (str (first cs))})))))))

(defn wrap-item-string-in-html [ts]
  (let [a-x-z (fn [t])]
    [:span (let [ts' (flatten
                      (s/map-on-binary-partitions
                       :id ts
                       identity #(str/join (map :text %))))]
             (for [t ts'
                   :let [id (:id t)]]
               (if-not id
                 t
                 [:a {:href (str \# (id/encode-id id))}
                  (:text t)])))]))

(defn- wrap-entry-in-html [[x & xs]]
  (case (:token x)
    :条 [:section {:class "entry" :id (id/encode-id (str \条 (:nth x)))}
         [:div {:class "title"}
          [:p (:text x)]]
         (for [x xs] (wrap-entry-in-html x))]

    :目 [:div {:class "目" :id (id/entry-id (:context x) :目)}
         [:p
          [:span (str (:nth x) \uFF0E)] ;use fullwith full stop
          (s/map-on-binary-partitions
           #(= (:token %) :to-be-recognized)
           (within-款项 (:context x) (seq (:text x)))
           #(str/join (map :text %))
           wrap-item-string-in-html)]]

    [:div {:class (name (:token x)) :id (id/entry-id (:context x) (:token x))}
     (for [line (str/split-lines (:text x))]
       [:p (s/map-on-binary-partitions
            #(= (:token %) :to-be-recognized)
            (within-款项 (:context x) (seq line))
            #(str/join (map :text %))
            wrap-item-string-in-html)])
     (for [x xs] (wrap-entry-in-html x))]))

(defn- wrap-条-in-html [head body]
  (assert (= (:token head) :条))
  (wrap-entry-in-html (条-rise (cons head body))))

(defn- outline-html [ts]
  (letfn [(max-hier [hval ts]
            (apply max (remove nil? (map (pt/hierachy-fn hval) ts))))
          (rise-ts [hval ts]
            (pt/linear-to-tree
             (cons {:token :pseudo-root} ts)
             (pt/hierachy-fn
              (merge hval {:序言 (max-hier hval ts)
                           :pseudo-root (inc (apply max (vals hval)))}))))
          (li [{ty :token :as t}]
            [:li
             [:a {:href (str "#" (id/entry-id (:context t) ty))}
              (:text t)]])
          (to-html [ot]
            (let [t (pt/node-val ot)
                  r (when (pt/internal-node? ot)
                      [:ul (for [li (pt/subtrees ot)]
                             (to-html li))])]
              (cond->> r
                (seq (:text t)) (conj (li t)))))]
    (to-html
     (pt/create
      (rise-ts {:节 1 :章 2 :则 3} ts)))))

(def ^:private draw-skeleton-with-contexts
  (comp ln/inject-contexts ln/draw-skeleton))

(defn- wrap-outline-in-html [outline]
  (assert (= (:token outline) :table-of-contents))
  (let [kv {:则 1 :章 2 :节 3}
        head (:text outline)
        item-list (draw-skeleton-with-contexts (:list outline))]
    [:section
     [:h2 {:id (id/encode-id "章0") :class "章"} head]
     [:nav {:id "outline" :class "entry"}
      (outline-html item-list)]]))

(defn- wrap-序言-in-html [t ts]
  (assert (= (:token t) :序言))
  [:section {:id (id/entry-id {} :序言) :class "entry"}
   [:h2 {:class "章"}
    (:text t)]
   (for [{tx :text} ts]
     [:div {:class "款"}
      [:p tx]])])

(defn- html-head [title css & scripts]
  [:head {:lang "zh-CN"}
   [:meta {:charset "utf-8"}]
   [:meta {:name "viewport"
           :content "width=device-width, initial-scale=1"}]
   [:title title]
   [:link {:rel "stylesheet" :href css}]
   (for [s scripts]
     [:script {:src s}])])

(defn- add-html-class
  {:test
   #(let [f add-html-class]
      (tt/comprehend-tests
       (t/is (= [:div {:class "a"}] (f [:div] "a")))
       (t/is (= [:div {:class "a b"}] (f [:div {:class "a"}] "b")))
       (t/is (= [:div {:class "a"} [:span "hello"]]
                (f [:div [:span "hello"]] "a")))))}
  [elmt class-name]
  (assert (and (first elmt) (seq class-name)))
  (let [[attrs-map children] (let [body (rest elmt)]
                               (if (map? (first body))
                                 [(first body) (rest body)]
                                 [{} body]))
        set-class (fn [s t]
                    (if (empty? s)
                      t
                      (str s " " t)))]
    (vec
     (cons (first elmt)
           (cons (update attrs-map :class set-class class-name)
                 children)))))

(defn- wrap-in-html [page-name link-to-original tokenized-lines]
  (html
   (html5
    (html-head page-name
               "index.css"
               "main.js"
               "ganalytics.js")
    [:body
     [:article {:class "entries-container"
                :onclick "void(0)" ; for iOS compatibility
                }
      [:div {:class "entry"}
       [:p {:id "ref-to-original" :class "not-in-original-text"}
        "原文请见："
        [:a {:href link-to-original :target "_blank"}
         (str (-> link-to-original
                  clojure.java.io/as-url
                  .getHost)
              "/……")]]]
      (seq
       (loop [tls tokenized-lines
              elmts []]
         (if (empty? tls)
           elmts
           (let [{t :token ct :context :as tl} (first tls)]
             (cond
               (= t :条)
               (let [[lines-within lines-after]
                     (split-with #(#{:款 :项 :目} (:token %)) (rest tls))]
                 (recur lines-after
                        (conj elmts (wrap-条-in-html tl lines-within))))

               (= t :序言)
               (let [[lines-within lines-after]
                     (split-with #(= (:token %) :to-be-recognized) (rest tls))]
                 (recur lines-after
                        (conj elmts (wrap-序言-in-html tl lines-within))))

               (#{:title :table-of-contents :则 :章 :节 :to-be-recognized} t)
               (let [txt (:text tl)
                     elmt (case t
                            :title
                            [:div [:h1 {:id "the-title"} txt] [:hr]]

                            :table-of-contents
                            (wrap-outline-in-html tl)

                            :则
                            [:h2 {:id (id/entry-id ct :则) :class "章"} txt]

                            :章
                            [:h2 {:id (id/entry-id ct :章) :class "章"} txt]

                            :节
                            [:h3 {:id (id/entry-id ct :节) :class "节"} txt]

                            :to-be-recognized
                            (default-fn txt))
                     wrapped-elmt (cond-> elmt
                                    (:not-in-original-text tl)
                                    (add-html-class "not-in-original-text"))]
                 (recur (rest tls) (conj elmts wrapped-elmt))))))))]
     [:a {:id "back-button"} "返回"]
     [:button {:id "share-button"} "分享"]
     [:div {:id "overlay"}
      [:div {:class "entries-container"}
       [:textarea {:id "share-text" :maxlength "1024"}]
       [:div {:id "overlay-button-panel"}
        [:button {:id "do-copy"} "完成"]
        [:button {:id "cancel-overlay"} "取消"]]]]])))

(defn- tokenized-lines [n ls]
  (let [[before-ts after-ls] (ln/recognize-title ls n)]
    (into
     before-ts
     (let [ls' (if (seq before-ts) after-ls ls)
           [before-ts' after-ls'] (ln/recognize-table-of-contents ls')]
       (if (seq before-ts')
         (into before-ts' (draw-skeleton-with-contexts after-ls'))
         ;;otherwise, table of contents is not found
         ;;generate it automatically
         (let [tls (draw-skeleton-with-contexts ls')
               [prelude toc] (ln/generate-table-of-contents tls)]
           (if (nil? toc) tls
               (concat prelude [toc] (s/without-prefix tls prelude)))))))))

(defn- index-page [entry-paths]
  (html
   (html5
    (html-head "法律文本富网页化"
               "index.css"
               "ganalytics.js")
    [:body
     [:div {:class "entries-container"}
      [:nav {:id "outline"}
       [:ul {:class "entry"}
        (for [[n p] entry-paths]
          [:li [:a {:href p} n]])]]]])))

(defn- txt->page [n]
  [(str n ".txt")
   (str n ".html")])

(defn- mainfn [names]
  (let [f (memoize txt->page)]
    (dorun
     ;; create all the document pages
     (for [[n l] names
           :let [[in out] (f n)]]
       (with-open [r (io/reader (io/file (io/resource in)))]
         (->> (line-seq r)
              (remove str/blank?)
              (map (comp use-chinese-paren space-clapsed str/trim))
              (tokenized-lines n)
              (wrap-in-html n l)
              (spit (str "../" out))))))
    ;; create the index page
    (spit "../index.html"
          (index-page (for [[n _] names
                            :let [[_ out] (f n)]]
                        [n out])))))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (mainfn
   [
    ["劳动合同法"
     "http://www.npc.gov.cn/wxzl/gongbao/2013-04/15/content_1811058.htm"]

    ["网络预约出租汽车经营服务管理暂行办法"
     "http://zizhan.mot.gov.cn/zfxxgk/bnssj/zcfgs/201607/t20160728_2068633.html"]

    ["高等教育法"
     "http://www.moe.edu.cn/publicfiles/business/htmlfiles/moe/moe_619/200407/1311.html"]

    ["种子法"
     "http://www.forestry.gov.cn/Zhuanti/content_lqgg/817306.html"]

    ["体育法"
     "http://www.sport.gov.cn/n16/n1092/n16819/312031.html"]

    ["婚姻法"
     "http://www.npc.gov.cn/wxzl/gongbao/2001-05/30/content_5136774.htm"]

    ["合同法"
     "http://www.npc.gov.cn/wxzl/wxzl/2000-12/06/content_4732.htm"]

    ["民法通则"
     "http://www.npc.gov.cn/wxzl/wxzl/2000-12/06/content_4470.htm"]

    ["网络借贷信息中介机构业务活动管理暂行办法"
     "http://www.cbrc.gov.cn/chinese/home/docDOC_ReadView/D934AAE7E05849D185CD497936D767CF.html"]

    ["互联网广告管理暂行办法"
     "http://www.saic.gov.cn/zwgk/zyfb/zjl/xxzx/201607/t20160708_169638.html"]

    ["个体工商户条例"
     "http://www.gov.cn/zwgk/2011-04/28/content_1853972.htm"]

    ["出口退（免）税企业分类管理办法"
     "http://www.chinatax.gov.cn/n810341/n810755/c2217201/content.html"]

    ["食品安全法"
     "http://www.gov.cn/zhengce/2015-04/25/content_2853643.htm"]

    ["立法法"
     "http://www.npc.gov.cn/npc/dbdhhy/12_3/2015-03/18/content_1930713.htm"]

    ["宪法"
     "http://www.npc.gov.cn/npc/xinwen/node_505.htm"]
    ]))

(-main)

(let [items (comp first tk/read-items)]
  (tt/comprehend-tests
   (for [src tk/item-string-examples
         :let [r (tk/parse (items src))]]
     (do
       ;; (clojure.pprint/pprint r)
       ;; (println "------------------------")
       (t/is (= src (str/join (map tk/str-token (flatten r)))))))
   (let [r (tk/parse (items (seq "本法第三十九条和第四十条第一项、第二项")))]
     (t/is
      (= '({:token :法 :nth :this :text "本法"}
           ({:token :条 :nth 39 :text "三十九" :第? true :unit? true :id "条39"}
            {:token :separator :text "和"})
           ({:token :条 :nth 40 :text "四十" :第? true :unit? true}
            ({:token :款 :nth 1}
             ({:token :项 :nth 1 :text "一" :第? true :unit? true :id "条40款1项1"}
              {:token :separator :text "、"})
             ({:token :项 :nth 2 :text "二" :第? true :unit? true :id "条40款1项2"}))))
         (pt/update-leaves r :id (partial id/generate {})))))
   (let [r (tk/parse (items (seq "本规定第十、十八、二十六、二十七条")))]
     (t/is
      (= '({:token :规定 :nth :this :text "本规定"}
           ({:token :条 :nth 10 :text "十" :第? true :unit? false :id "条10"}
            {:token :separator :text "、"})
           ({:token :条 :nth 18 :text "十八" :第? false :unit? false :id "条18"}
            {:token :separator :text "、"})
           ({:token :条 :nth 26 :text "二十六" :第? false :unit? false :id "条26"}
            {:token :separator :text "、"})
           ({:token :条 :nth 27 :text "二十七" :第? false :unit? true :id "条27"}))
         (pt/update-leaves r :id (partial id/generate {})))))))
