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
       [(t/is (= [["目 录"] ["目 录"]] (table-of-contents ["目 录"])))
        (t/is (= [["目 录" "第一章" "第二章" "第三章"]
                  ["目 录" "第一章" "第二章" "第三章"]]
                 (table-of-contents txt)))]))}
  [ls]
  (let [head (first ls)]
    (assert (re-matches table-of-contents-sentinel head))
    (if-let [first-item (second ls)]
      (loop [s (rest (rest ls))
             t [head first-item]]
        (if (or (= (first s) first-item) (empty? s))
          [t t]
          (recur (rest s) (conj t (first s)))))
      [[head] [head]])))

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

(defn- wrap-in-html [lines]
  (let [title (first lines)]
    (html
     (html5
      [:head {:lang "zh"}
       [:meta {:charset "utf-8"}]
       [:meta {:name "viewport"
               :content "width=device-width, initial-scale=1"}]
       [:title title]]
      [:body
       (loop [ls lines
              es []
              env {}]
         (cond (empty? ls)
               (seq es)

               (re-matches table-of-contents-sentinel (first ls))
               (let [[processed [head & item-list]] (table-of-contents ls)]
                 (recur (without-prefix ls processed)
                        (conj es [:nav [:h2 head]
                                  [:ul (for [item item-list]
                                         [:li [:a {:href (str "#" (space-filled item))}
                                               item]])]])
                        (assoc env :table-of-contents item-list)))

               (let [[_ unit] (nth-item (first ls))]
                 (= unit \章))
               (let [[processed {:keys [title content]}] (token/章 ls)]
                 (recur (without-prefix ls processed)
                        (conj es [:div {:id (space-filled title)}
                                  [:h2 title]
                                  (for [line content] [:p line])])
                        env))

               :else
               (recur (rest ls) (conj es (default-fn (first ls))) env)))]))))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (with-open [r (io/reader "../original_text.txt")]
    (->> (line-seq r)
         (remove str/blank?)
         (map (comp use-chinese-paren space-clapsed str/trim))
         wrap-in-html
         (spit "../index.html"))))

(-main)
