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
               #{[\办 \法] [\规 \定] [\法] [\条] [\款]}
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
      (rise-ts {:节 1 :章 2 :则 3 :编 3} ts)))))

(def ^:private draw-skeleton-with-contexts
  (comp ln/inject-contexts ln/draw-skeleton))

(defn- wrap-outline-in-html [outline]
  (assert (= (:token outline) :table-of-contents))
  (let [head (:text outline)
        item-list (draw-skeleton-with-contexts (:list outline))]
    [:section
     [:h2 {:id (id/encode-id "编0") :class "编"} head]
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
   [:link {:rel "icon" :href "favicon.png"}]
   [:link {:rel "stylesheet" :href css}]
   (for [s scripts]
     [:script {:src s}])
   [:script {:async true :src "ganalytics.js"}]
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
               "main.js")
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

               (#{:title :table-of-contents :编 :则 :章 :节 :to-be-recognized} t)
               (let [txt (:text tl)
                     elmt (case t
                            :title
                            [:div [:h1 {:id "the-title"} txt] [:hr]]

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
               "index.css")
    [:body
     [:div {:class "entries-container"}
      [:nav {:id "outline"}
       [:ul {:class "entry"}
        (for [[n p] entry-paths]
          [:li [:a {:href p} n]])]]]])))

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
             ({:token :项 :nth 2 :text "二" :第? true :unit? true :id "条40款1项2"}))))
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
           ({:token :条 :nth 27 :text "二十七" :第? false :unit? true :id "条27"}))
         (pt/update-leaves r :id (partial id/generate {})))))))
