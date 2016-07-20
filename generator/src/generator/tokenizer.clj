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

(defn nth-章节
  {:test
   #(let [f nth-章节]
      (tt/comprehend-tests
       (t/is (= {:token \章 :nth 1 :text "第一章 总则"}
                (f ["第一章 总则" "第一条 ……"])))
       (t/is (= {:token \节 :nth 2 :text "第二节 劳务派遣"}
                (f ["第二节 劳务派遣" "第五十七条 ……"])))))}
  [lines]
  (let [line (first lines)
        [i unit] (nth-item line)]
    (assert (and i (#{\章 \节} unit)))
    {:token unit :nth i :text line}))

(defn nth-条
  {:test
   #(let [f nth-条
          a ["第五条 县级以上……"]
          b ["第六条 工会应当……" "……"]
          c ["第二章 ……"]
          d ["第七条 用人单位……"]
          ab (into a b)
          bcd (-> b (into c) (into d))]
      (tt/comprehend-tests
       (t/is (= [a {:token \条 :nth 5 :text a :head "第五条"}]
                (f ab)))
       (t/is (= [b {:token \条 :nth 6 :text b :head "第六条"}]
                (f bcd)))))}
  [lines]
  (let [line (first lines)
        [i unit] (nth-item line)]
    (assert (and i (= unit \条)))
    (let [lines-within
          (into [line] (take-while
                        (fn [l]
                          (let [[_ unit] (nth-item l)]
                            (nil? (#{\章 \节 \条} unit))))
                        (rest lines)))]
      [lines-within
       {:token \条 :nth i :head (first (str/split line #"\s"))
        :text lines-within}])))
