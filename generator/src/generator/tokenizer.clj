(ns generator.tokenizer
  (:require [clojure.string :as str]
            [clojure.test :as t]
            [generator.test :as tt]))

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

(defn 章
  {:test
   #(let [f 章
          a ["第七章 法律责任" "第八十条 ……" "……"]
          b ["第八章 附则" "第九十六条 ……"]
          ab (into a b)]
      (tt/comprehend-tests
       (let [[processed recognized] (f ab)]
         (println (f ab))
         [(t/is (= a processed))
          (t/is (= {:token \章 :nth 7 :title (first a) :content (rest a)}
                   recognized))])
       (let [[processed recognized] (f b)]
         (println (f b))
         [(t/is (= b processed))
          (t/is (= {:token \章 :nth 8 :title (first b) :content (rest b)}))])))}
  [lines]
  (let [head (first lines)
        [i unit] (nth-item head)]
    (assert (and i (= unit \章)))
    (let [next [(inc i) \章]
          body (take-while (fn [line]
                             (not= (nth-item line) next))
                           (rest lines))]
      [(cons head body) {:token \章 :nth i :title head :content body}])))
