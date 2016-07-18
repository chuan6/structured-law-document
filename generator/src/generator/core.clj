(ns generator.core
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :as t]
            [generator.test :as tt]
            [hiccup.core :refer :all]
            [hiccup.page :refer :all])
  (:gen-class))

(defn table-of-contents
  {:test
   #(let [txt ["标题" "目 录" "第一章" "第二章" "第三章" "第一章" "……"]]
      (t/is (= ["目 录" "第一章"
                "第二章" "第三章"] (table-of-contents txt))))}
  [ls]
  (let [sentinel   #"目\s*录"
        skipped    (drop-while #(nil? (re-matches sentinel %)) ls)
        first-item (second skipped)]
    (assert (nil? (re-matches sentinel first-item)))
    (->> skipped
         (partition-by #(not= % first-item))
         (take 3)
         flatten)))

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
       [(t/is (= "az" (f "az")))
        (t/is (= "a z" (f "a z")))
        (t/is (= "a z" (f "a  z")))]))}
  [s]
  (str/join " " (str/split s #"\s+")))

(defn- wrap-in-html [ls]
  (let [title (first ls)]
    (html
     (html5
      [:head {:lang "zh"}
       [:meta {:charset "utf-8"}]
       [:meta {:name "viewport" :content "width=device-width, initial-scale=1"}]
       [:title title]]
      [:body
       (let [[head & item-list] (table-of-contents ls)]
         [:nav [:h2 head]
          [:ul (for [item item-list] [:li item])]])
       (map (partial conj [:p]) ls)]))))

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
