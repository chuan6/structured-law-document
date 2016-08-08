(ns generator.zh-digits
  (:require [clojure.test :as t]
            [generator.test :as tt]))

(def ^:private cdigit-map
  {\零 0 \一 1 \二 2 \三 3 \四 4 \五 5 \六 6 \七 7 \八 8 \九 9})

(def ^:private nthten-map
  {\十 10 \百 100})

(def numchar-zh-set (set (concat (keys cdigit-map)
                                 (keys nthten-map))))

(defn 数字
  {:test
   #(let [f 数字]
      (tt/comprehend-tests
       (t/is (= [0 []]                      (f "")))
       (t/is (= [0 [\零]]                   (f "零")))
       (t/is (= [1 [\一]]                   (f "一")))
       (t/is (= [10 [\十]]                  (f "十")))
       (t/is (= [12 [\十 \二]]              (f "十二章")))
       (t/is (= [20 [\二 \十]]              (f "二十行")))
       (t/is (= [34 [\三 \十 \四]]          (f "三十四")))
       (t/is (= [567 [\五 \百 \六 \十 \七]] (f "五百六十七")))
       (t/is (= [809 [\八 \百 \零 \九]]     (f "八百零九回")))))}
  [s]
  (let [c (first s)]
    (cond (empty? s) [0 []]
          (= c \十)  (let [[x s'] (数字 (rest s))]
                       [(+ 10 x) (cons \十 s')])
          (= c \零)  (let [[x s'] (数字 (rest s))]
                       [x (cons \零 s')])

          (contains? cdigit-map c)
          (let [d (cdigit-map c)
                n (second s)]
            (if (contains? nthten-map n)
              (let [[x s'] (数字 (nthrest s 2))]
                [(+ (* d (nthten-map n)) x) (into [c n] s')])
              [d [c]]))

          :else [0 []])))
