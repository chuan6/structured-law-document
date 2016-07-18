(ns generator.core
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :as t]
            [generator.test :as tt]
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

(def cdigit-map
  {\零 0 \一 1 \二 2 \三 3 \四 4 \五 5 \六 6 \七 7 \八 8 \九 9})

(def nthten-map
  {\十 10 \百 100})

(defn chinese-number
  {:test
   #(let [f chinese-number]
      (tt/comprehend-tests
       (t/is (= 0   (f "零")))
       (t/is (= 1   (f "一")))
       (t/is (= 10  (f "十")))
       (t/is (= 12  (f "十二")))
       (t/is (= 20  (f "二十行")))
       (t/is (= 34  (f "三十四")))
       (t/is (= 567 (f "五百六十七")))
       (t/is (= 809 (f "八百零九回")))))}
  [s]
  (let [c (first s)]
    (cond (empty? s) 0
          (= c \十)  (+ 10 (chinese-number (rest s)))
          (= c \零)  (chinese-number (rest s))

          (contains? cdigit-map c)
          (let [d (cdigit-map c)
                n (second s)]
            (if (contains? nthten-map n)
              (+ (* d (nthten-map n)) (chinese-number (nthrest s 2)))
              d))

          :else 0)))

(defn nth-item
  {:test
   #(let [f nth-item]
      (tt/comprehend-tests
       (t/is (= [1 \章] (f "第一章 总则")))
       (t/is (= [12 \条] (f "第十二条 ……")))))}
  [line]
  (let [[c & cs] line
        cd-set (set (concat (keys cdigit-map) (keys nthten-map)))]
    (when (= c \第)
      (let [[head tail] (split-with cd-set cs)]
        (when (seq head)
          [(chinese-number head) (first tail)])))))

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
              es []]
         (cond (empty? ls)
               (seq es)

               (re-matches table-of-contents-sentinel (first ls))
               (let [[processed [head & item-list]] (table-of-contents ls)]
                 (recur (without-prefix ls processed)
                        (conj es [:nav [:h2 head]
                                  [:ul (for [item item-list] [:li item])]])))

               :else
               (recur (rest ls) (conj es (default-fn (first ls))))))]))))

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
