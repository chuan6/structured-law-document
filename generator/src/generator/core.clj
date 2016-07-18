(ns generator.core
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :as t]
            [generator.test :as tt]
            [hiccup.core :refer :all]
            [hiccup.page :refer :all])
  (:gen-class))

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
