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
  [:section {:class "entry" :id (encode-id (str \条 (:nth head)))}
   [:div {:class "title"}
    [:b (:text head)]]
   (seq
    (loop [ps []
           [t & ts] body
           i-款 0]
      (if (nil? t)
        ps
        (condp = (:token t)
          :款 (recur (conj ps [:p {:class "款"
                                   :id (encode-id (str "条" (:nth head)
                                                        "款" (inc i-款)))}
                               (s/map-on-binary-partitions
                                #(= (:token %) :to-be-recognized)
                                (within-款项 {:条 (:nth head)
                                              :款 (inc i-款)} (seq (:text t)))
                                #(str/join (map :text %))
                                wrap-item-string-in-html)])
                     ts
                     (inc i-款))
          :项 (recur (conj ps [:p {:class "项"
                                   :id (encode-id (str "条" (:nth head)
                                                        "款" i-款
                                                        "项" (:nth t)))}
                               (s/map-on-binary-partitions
                                #(= (:token %) :to-be-recognized)
                                (within-款项 {:条 (:nth head)
                                              :款 i-款} (seq (:text t)))
                                #(str/join (map :text %))
                                wrap-item-string-in-html)])
                     ts
                     i-款)))))])

(defn- gen-outline-id [t]
  (encode-id (str (name (:token t)) (:nth t))))

(defn- outline-html [ts gen-id]
  (let [level (fn [t] ((:token t) {:节 1 :章 2 :则 3}))
        list-item (fn [t] [:li [:a {:href (str "#" (gen-id t))}
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

(defn- wrap-outline-in-html [outline]
  (assert (= (:token outline) :table-of-contents))
  (let [kv {:则 1 :章 2 :节 3}
        head (:text outline)
        item-list (l/draw-skeleton (:list outline))]
    [:section
     [:h2 {:id (encode-id "章0") :class "章"} head]
     [:nav {:id "outline" :class "entry"}
      (first (outline-html item-list gen-outline-id))]]))

(defn- wrap-in-html [tokenized-lines]
  (html
   (html5
    [:head {:lang "zh-CN"}
     [:meta {:charset "utf-8"}]
     [:meta {:name "viewport"
             :content "width=device-width, initial-scale=1"}]
     [:title (:text (first tokenized-lines))]
     [:link {:rel "stylesheet" :href "index.css"}]
     [:script {:src "main.js"}]
     [:script {:src "ganalytics.js"}]]
    [:body
     [:article {:class "entries-container"
                :onclick "void(0)" ; for iOS compatibility
                }
      (seq
       (loop [tls tokenized-lines
              elmts []]
         (if (empty? tls)
           elmts
           (let [{t :token :as tl} (first tls)]
             (case t
               :table-of-contents
               (recur (rest tls)
                      (conj elmts (wrap-outline-in-html tl)))

               :则
               (let [txt (:text tl)]
                 (recur (rest tls)
                        (conj elmts [:h2 {:id (gen-outline-id tl)
                                          :class "章"}
                                     txt])))
               :章
               (let [txt (:text tl)]
                 (recur (rest tls)
                        (conj elmts [:h2 {:id (gen-outline-id tl)
                                          :class "章"}
                                     txt])))

               :节
               (let [txt (:text tl)]
                 (recur (rest tls)
                        (conj elmts [:h3 {:id (gen-outline-id tl)
                                          :class "节"}
                                     txt])))

               :条
               (let [[lines-within lines-after]
                     (split-with #(#{:款 :项} (:token %)) (rest tls))]
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

(defn- tokenized-lines [ls]
  (let [[before-ts after-ls] (l/recognize-table-of-contents ls)]
    (if (seq before-ts)
      (into before-ts (l/draw-skeleton after-ls))
      ;;otherwise, table of contents is not found
      ;;generate it automatically
      (-> ls l/draw-skeleton l/attach-table-of-contents))))

(defn- mainfn [inname->outpath]
  (dorun
   (for [[in out] inname->outpath]
     (with-open [r (io/reader (io/file (io/resource in)))]
       (->> (line-seq r)
            (remove str/blank?)
            (map (comp use-chinese-paren space-clapsed str/trim))
            tokenized-lines
            wrap-in-html
            (spit out))))))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (mainfn {"劳动合同法.txt"
           "../劳动合同法.html"

           "网络预约出租汽车经营服务管理暂行办法.txt"
           "../网络预约出租汽车经营服务管理暂行办法.html"

           "高等教育法.txt"
           "../高等教育法.html"

           "种子法.txt"
           "../种子法.html"

           "体育法.txt"
           "../体育法.html"

           "婚姻法.txt"
           "../婚姻法.html"

           "合同法.txt"
           "../合同法.html"

           "民法通则.txt"
           "../民法通则.html"
           }))

(-main)
