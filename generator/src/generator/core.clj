(ns generator.core
  (:require [clj-http.util :as http]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :as t]
            [generator.line :as l]
            [generator.lisp :as s]
            [generator.test :as tt]
            [generator.tokenizer :as tk]
            [generator.zh-digits :refer [numchar-zh-set]]
            [hiccup.core :refer :all]
            [hiccup.page :refer :all])
  (:gen-class))

(defn default-fn [l] [:p l])

(defn- encode-id [s]
  (str/replace (http/url-encode s) #"%" "."))

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

(defn- entry-id [context t]
  (letfn [(gen-str [context t]
            (let [r (str (name t) (t context))]
              (case t
                :则 r
                :章 r
                :节 (str (gen-str context :章) r)
                :条 r
                :款 (str (gen-str context :条) r)
                :项 (str (gen-str context :款) r)
                :目 (str (gen-str context :项) r))))]
    (encode-id (gen-str context t))))

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
  (let [genid (partial tk/generate-id context)
        flags [#{[\本] [\前]}
               #{[\规 \定] [\法] [\条] [\款]}
               #{[\第]}]]
    (loop [cs cs ts []]
      (if (empty? cs)
        ts
        (if (s/seq-match flags cs)
          (let [[items rests] (tk/read-items cs)]
            (recur rests (into ts (-> items
                                      tk/parse
                                      (tk/update-leaves :id genid)
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
                 [:a {:href (str \# (encode-id id))}
                  (:text t)])))]))

(defn- wrap-条-in-html [head body]
  (assert (= (:token head) :条))
  (let [attr-and-content
        (fn [t]
          [;;attributes
           {:class (name (:token t))
            :id (entry-id (:context t) (:token t))}
           ;;content
           (s/map-on-binary-partitions
            #(= (:token %) :to-be-recognized)
            (within-款项 (:context t) (seq (:text t)))
            #(str/join (map :text %))
            wrap-item-string-in-html)])]
   [:section {:class "entry" :id (encode-id (str \条 (:nth head)))}
    [:div {:class "title"}
     [:b (:text head)]]
    (s/map-on-binary-partitions
     #(= (:token %) :目)
     body
     (fn create-ol [items]
       [:ol
        (for [it items]
          (into [:li] (attr-and-content it)))])
     (fn create-ps [items]
       (for [it items]
         (into [:p] (attr-and-content it)))))]))

(defn- outline-html [ts gen-id]
  (let [level (fn [t] ((:token t) {:节 1 :章 2 :则 3}))
        list-item (fn [t] [:li [:a {:href (str "#" (gen-id (:context t)
                                                           (:token t)))}
                                (:text t)]])]
   (if (empty? ts)
     ()
     (let [curr (level (first ts))
           [xs ys] (split-with #(= (level %) curr) ts)]
       (loop [elmt [:ul (for [x xs] (list-item x))]
              ys ys]
         (if (empty? ys)
           [elmt ()]
           (let [next (level (first ys))]
             (cond (> next curr)
                   [elmt ys]

                   (= next curr)
                   (recur (conj elmt (list-item (first ys))) (rest ys))

                   ;;(< next curr)
                   :else
                   (let [[sub-elmt ys'] (outline-html ys gen-id)
                         elmt' (conj elmt sub-elmt)]
                     (recur elmt' ys'))))))))))

(def ^:private draw-skeleton-with-contexts
  (comp l/inject-contexts l/draw-skeleton))

(defn- wrap-outline-in-html [outline]
  (assert (= (:token outline) :table-of-contents))
  (let [kv {:则 1 :章 2 :节 3}
        head (:text outline)
        item-list (draw-skeleton-with-contexts (:list outline))]
    [:section
     [:h2 {:id (encode-id "章0") :class "章"} head]
     [:nav {:id "outline" :class "entry"}
      (first (outline-html item-list entry-id))]]))

(defn- html-head [title css & scripts]
  [:head {:lang "zh-CN"}
   [:meta {:charset "utf-8"}]
   [:meta {:name "viewport"
           :content "width=device-width, initial-scale=1"}]
   [:title title]
   [:link {:rel "stylesheet" :href css}]
   (for [s scripts]
     [:script {:src s}])])

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
       [:p {:id "ref-to-original"}
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
             (case t
               :title
               (let [txt (:text tl)]
                 (recur (rest tls)
                        (into elmts
                              [[:h1 {:id "the-title"} txt]
                               [:hr]])))

               :table-of-contents
               (recur (rest tls)
                      (conj elmts (wrap-outline-in-html tl)))

               :则
               (let [txt (:text tl)]
                 (recur (rest tls)
                        (conj elmts [:h2 {:id (entry-id ct :则)
                                          :class "章"}
                                     txt])))
               :章
               (let [txt (:text tl)]
                 (recur (rest tls)
                        (conj elmts [:h2 {:id (entry-id ct :章)
                                          :class "章"}
                                     txt])))

               :节
               (let [txt (:text tl)]
                 (recur (rest tls)
                        (conj elmts [:h3 {:id (entry-id ct :节)
                                          :class "节"}
                                     txt])))

               :条
               (let [[lines-within lines-after]
                     (split-with #(#{:款 :项 :目} (:token %)) (rest tls))]
                 (recur lines-after
                        (conj elmts (wrap-条-in-html tl lines-within))))

               :to-be-recognized
               (recur (rest tls)
                      (conj elmts (default-fn (:text tl)))))))))]
     [:a {:id "back-button"} "返回"]
     [:button {:id "share-button"} "分享"]
     [:div {:id "overlay"}
      [:div {:class "entries-container"}
       [:textarea {:id "share-text" :maxlength "1024"}]
       [:div {:id "overlay-button-panel"}
        [:button {:id "do-copy"} "完成"]
        [:button {:id "cancel-overlay"} "取消"]]]]])))

(defn- tokenized-lines [n ls]
  (let [[before-ts after-ls] (l/recognize-title ls n)]
    (into
     before-ts
     (let [ls' (if (seq before-ts) after-ls ls)
           [before-ts' after-ls'] (l/recognize-table-of-contents ls')]
       (if (seq before-ts')
         (into before-ts' (draw-skeleton-with-contexts after-ls'))
         ;;otherwise, table of contents is not found
         ;;generate it automatically
         (-> ls' draw-skeleton-with-contexts l/attach-table-of-contents))))))

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
   [["劳动合同法"
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
     "http://www.chinatax.gov.cn/n810341/n810755/c2217201/content.html"]]))

(-main)
