(ns generator.core
  (:require [aho-corasick.core :as acc]
            [cheshire.core :as json]
            [clj-http.util :as http]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :as t]
            [generator.id :as id]
            [generator.line :as ln]
            [generator.lisp :as s]
            [generator.punct :as punct]
            [generator.source :as src]
            [generator.test :as tt]
            [generator.toc :as toc]
            [generator.item-string :as its]
            [generator.parse-tree :as pt]
            [generator.zh-digits :refer [numchar-zh-set]]
            [hiccup.core :refer :all]
            [hiccup.page :refer :all])
  (:gen-class))

(defn default-fn [l] [:p l])

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
               #{[\办 \法] [\规 \定] [\法] [\章] [\节] [\条] [\款]}
               #{[\第]}]
        patterns ["宪法"]
        g (acc/construct patterns)

        prefix
        (fn [cs]
          (take (apply max (map count patterns)) cs))

        choose-matched
        (fn [{i :position ms :outputs}]
          (first (filter #(= (count %) (inc i)) ms)))]
    (loop [cs cs ts []]
      (if (empty? cs)
        ts
        (if-let [p (choose-matched
                    (first (acc/matching g (prefix cs))))]
          (recur (nthrest cs (count p)) (conj ts {:token :external-ref
                                                  :text p
                                                  :href (str p ".html")}))
          (if (s/seq-match flags cs)
            (let [[items rests] (its/read-items cs)]
              (recur rests (into ts (-> items
                                        its/parse
                                        (pt/update-leaves :id genid)
                                        flatten
                                        its/second-pass))))
            (recur (rest cs) (conj ts {:token :to-be-recognized
                                       :text (str (first cs))}))))))))

(defn wrap-item-string-in-html [[t :as ts]]
  (if (and (= (count ts) 1)
           (= (:token t) :external-ref))
    [:a {:href (:href t) :target "_blank"} (:text t)]
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
                    (:text t)])))])))

(defn- wrap-entry-in-html [[x & xs]]
  (case (:token x)
    :条 [:section {:class "entry"
                   :id (id/encode-id (str \条 (id/nth-str (:nth x))))}
         [:div {:class "entry-num n-i-o-t"}
          [:span (int (:nth x))]]
         [:div {:class "title"}
          [:p (:text x)]]
         (for [x xs] (wrap-entry-in-html x))]

    :目 [:div {:class "目"
               :id (id/entry-id (:context x) :目)}
         [:p
          [:span (str (:nth x) \uFF0E)] ;use fullwith full stop
          (s/map-on-binary-partitions
           #(= (:token %) :to-be-recognized)
           (within-款项 (:context x) (seq (:text x)))
           #(str/join (map :text %))
           wrap-item-string-in-html)]]

    [:div {:class (name (:token x))
           :id (id/entry-id (:context x) (:token x))}
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

(def ^:private draw-skeleton-with-contexts
  (comp ln/inject-contexts ln/draw-skeleton))

(defn- wrap-outline-in-html [outline]
  (assert (= (:token outline) :table-of-contents))
  (cond-> [:section {:id "toc"}]
    (:text outline) (conj [:h2 {:id (id/encode-id "编0") :class "编"}
                           (:text outline)])
    true            (conj [:nav {:id (id/encode-id "目录") :class "entry"}
                           (toc/outline-html (:list outline))])))

(defn- wrap-序言-in-html [t ts]
  (assert (= (:token t) :序言))
  [:section {:id (id/entry-id {} :序言) :class "entry"}
   [:h2 {:class "章"}
    (:text t)]
   (for [{tx :text} ts]
     [:p tx])])

(defn- html-head [title css & scripts]
  [:head {:lang "zh-CN"}
   [:meta {:charset "utf-8"}]
   [:meta {:name "viewport"
           :content "width=device-width, initial-scale=1"}]
   [:title title]
   [:link {:rel "icon" :href "favicon.png"}]
   [:link {:rel "stylesheet" :href css}]
   (for [s (conj scripts "ganalytics.js")]
     [:script {:async true :src s}])
   [:script {:type "application/ld+json"}
    (json/generate-string
     {"@context" "http://schema.org"
      "@type"    "WebSite"
      "name"     "读法.com"
      "url"      "https://读法.com"})]])

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
               "main.js" "qrcode_kazuhikoarase.js")
    [:body
     [:article {:class "entries-container"
                :onclick "void(0)" ; for iOS compatibility
                }
      [:div {:class "entry"}
       [:p {:id "ref-to-original" :class "n-i-o-t"}
        [:a {:href link-to-original :target "_blank"}
         "原文链接"]]]
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

               (#{:title :table-of-contents :编 :则 :章 :节 :to-be-recognized} t)
               (let [txt (:text tl)
                     elmt (case t
                            :title
                            [:div
                             (cond-> [:h1 {:id "the-title"}]
                               (seq (:head tl)) (into [(:head tl) [:br]])
                               true             (conj txt))
                             [:hr]]

                            :table-of-contents
                            (wrap-outline-in-html tl)

                            :编
                            [:h2 {:id (id/entry-id ct :编) :class "编"} txt]

                            :则
                            [:h2 {:id (id/entry-id ct :则) :class "编"} txt]

                            :章
                            [:h3 {:id (id/entry-id ct :章) :class "章"} txt]

                            :节
                            [:h4 {:id (id/entry-id ct :节) :class "节"} txt]

                            :to-be-recognized
                            (default-fn txt))
                     wrapped-elmt (cond-> elmt
                                    (:not-in-original-text tl)
                                    (add-html-class "n-i-o-t"))]
                 (recur (rest tls) (conj elmts wrapped-elmt))))))))
      [:div {:id "wxdyh_qrcode"}
       [:img {:src "images/wxdyh_qrcode.jpg"
              :alt "读法网微信订阅号"}]]]
     [:a {:id "back-button"} "返回"]
     [:button {:id "share-button" :style "display: none;"} "分享"]
     [:div {:id "overlay"}
      [:div {:class "entries-container"}
       [:textarea {:id "share-text" :maxlength "1024"}]
       [:div {:id "overlay-button-panel"}
        [:button {:id "do-copy"} "完成"]
        [:button {:id "cancel-overlay"} "取消"]]]]])))

(defn- tokenized-lines [n ls]
  (let [[title-and-above below-title] (ln/recognize-title ls n)]
    (into
     title-and-above
     (let [ls' (if (seq title-and-above) below-title ls)
           [toc-and-above below-toc] (toc/recognize-table-of-contents ls')]
       (if (seq toc-and-above)
         ;; ignore existing toc, and use the generated one
         (let [tls (draw-skeleton-with-contexts below-toc)
               [_ toc'] (toc/generate-table-of-contents tls)]
           (into (conj (pop toc-and-above) toc') tls))
         ;; use the generated one
         (let [tls (draw-skeleton-with-contexts ls')
               [prelude toc] (toc/generate-table-of-contents tls)]
           (assert (or (:text toc) (= toc toc/empty-toc)))
           (concat prelude [toc] (s/without-prefix tls prelude))))))))

(defn- structure-validated [tls]
  (letfn [(step [ty {i :nth :as prev} {j :nth :as curr}]
            (cond
              (and (integer? j) (not= (- j (int i)) 1))
              (println "ERROR expect" (id/nth-str (inc i)) "th"
                       ty "instead of" j "th" ty)

              (and (ratio? j) (not= j (ln/sub-inc i)))
              (println "ERROR expect" (id/nth-str (ln/sub-inc i)) "th"
                       ty "instead of" j "th" ty)

              (not (number? j))
              (println "ERROR :nth of" curr "is not a number"))
            curr)

          (counting [from-1? xs]
            (let [xs (remove nil? xs)]
              (when (seq xs)
                (let [x (first xs)
                      ty (:token x)]
                  (when (and from-1? (not= (:nth x) 1))
                    (println "ERROR expect the first" ty "to have"
                             ":nth value of 1 instead of" (:nth x)))
                  (reduce (partial step ty) xs)))))

          (partition-at-every-other [xs ty]
            (lazy-seq
             (when (seq xs)
               (let [[head tail] (if (= (:token (first xs)) ty)
                                   [(first xs) (rest xs)]
                                   [nil xs])
                     [tail-a tail-b] (split-with #(not= (:token %) ty) tail)]
                 (cons [head tail-a]
                       (partition-at-every-other tail-b ty))))))]
    (let [条s (filter #(= (:token %) :条) tls)
          编章节s (filter #(#{:编 :章 :节} (:token %)) tls)
          counting-from-1 (partial counting true)]
      (counting-from-1 条s)
      (counting-from-1
       (for [[b zs] (partition-at-every-other 编章节s :编)]
         (do (counting
              false
              (for [[z js] (partition-at-every-other zs :章)]
                (do (counting-from-1 js)
                    z)))
             b)))
      tls)))

(defn- index-page [entry-paths]
  (html
   (html5
    (html-head "法律文本富网页化"
               "index.css")
    [:body
     [:div {:class "entries-container"}
      [:nav {:id (id/encode-id "目录")}
       [:ul {:class "entry"}
        (for [[n p] entry-paths]
          [:li {:class "li-head"}
           [:a {:href p} n]])]]]])))

(defn- mainfn [names]
  (let [f (memoize src/txt-page-pair)]
    (dorun
     ;; create all the document pages
     (for [[n l] names
           :let [[in out] (f n)]]
       (do
         (println n)
         (with-open [r (io/reader (io/file (io/resource in)))]
           (->> (line-seq r)
                (remove str/blank?)
                (map (comp punct/use-chinese space-clapsed str/trim))
                (tokenized-lines n)
                (structure-validated)
                (wrap-in-html n l)
                (spit (str "../" out)))))))
    ;; create the index page
    (spit "../index.html"
          (index-page (for [[n _] names
                            :let [[_ out] (f n)]]
                        [n out])))))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (mainfn src/name-link-pairs))

(-main)

(let [items (comp first its/read-items)]
  (tt/comprehend-tests
   (for [src its/examples
         :let [r (its/parse (items src))]]
     (do
       ;; (clojure.pprint/pprint r)
       ;; (println "------------------------")
       (t/is (= src (str/join (map its/str-token (flatten r)))))))
   (let [r (its/parse (items (seq "本法第三十九条和第四十条第一项、第二项")))]
     (t/is
      (= '({:token :法 :nth :this :text "本法"}
           ({:token :条 :nth 39 :text "三十九" :第? true :unit? true :id "条39"}
            {:token :separator :text "和"})
           ({:token :条 :nth 40 :text "四十" :第? true :unit? true}
            ({:token :款 :nth 1}
             ({:token :项 :nth 1 :text "一" :第? true :unit? true :id "条40款1项1"}
              {:token :separator :text "、"})
             {:token :项 :nth 2 :text "二" :第? true :unit? true :id "条40款1项2"})))
         (pt/update-leaves r :id (partial id/generate {})))))
   (let [r (its/parse (items (seq "本规定第十、十八、二十六、二十七条")))]
     (t/is
      (= '({:token :规定 :nth :this :text "本规定"}
           ({:token :条 :nth 10 :text "十" :第? true :unit? false :id "条10"}
            {:token :separator :text "、"})
           ({:token :条 :nth 18 :text "十八" :第? false :unit? false :id "条18"}
            {:token :separator :text "、"})
           ({:token :条 :nth 26 :text "二十六" :第? false :unit? false :id "条26"}
            {:token :separator :text "、"})
           {:token :条 :nth 27 :text "二十七" :第? false :unit? true :id "条27"})
         (pt/update-leaves r :id (partial id/generate {})))))))
