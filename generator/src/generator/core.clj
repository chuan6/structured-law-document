(ns generator.core
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :as t]
            [hiccup.core :refer :all]
            [hiccup.page :refer :all])
  (:gen-class))

(defn- wrap-in-html [ls]
  (let [trimed-ls (map str/trim ls)
        title (first trimed-ls)]
    (html
     (html5
      [:head {:lang "zh"}
       [:meta {:charset "utf-8"}]
       [:meta {:name "viewport" :content "width=device-width, initial-scale=1"}]
       [:title title]]
      [:body
       (map (partial conj [:p]) trimed-ls)]))))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (with-open [r (io/reader "../original_text.txt")]
    (->> (line-seq r)
         (filter (comp not str/blank?))
         wrap-in-html
         (spit "../index.html"))))

(-main)
