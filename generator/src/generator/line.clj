(ns generator.line
  (:require [clojure.string :as str]
            [clojure.test :as t]
            [generator.lisp :as s]
            [generator.test :as tt]
            [generator.zh-digits :refer [数字 numchar-zh-set]]))

(defn nth-item
  {:test
   #(let [f nth-item]
      (tt/comprehend-tests
       (t/is (= [1 \章] (f "第一章 总则")))
       (t/is (= [12 \条] (f "第十二条 ……")))))}
  [line]
  (let [[c & cs] line]
    (when (= c \第)
      (let [[i processed] (数字 cs)]
        [i (first (s/without-prefix cs processed))]))))

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

(defn 括号数字
  {:test
   #(let [f 括号数字]
      (tt/comprehend-tests
       (t/is (not (f "（")))
       (t/is (not (f "（）")))
       (t/is (not (f "（括号内）")))
       (t/is (= [1 [\（ \一 \）]] (f "（一）……")))))}
  [cs]
  (let [openfn (partial s/from-x \（)
        closefn (partial s/to-x \）)]
    (when-let [[begin body end] (s/read-xs openfn closefn cs)]
      (let [[i processed] (数字 body)]
        (when (and (seq processed) (= processed body))
          [i (s/flatten-and-vector begin body end)])))))

(defn nth-项
  {:test
   #(let [f nth-项]
      (tt/comprehend-tests
       (t/is (= nil (f "（")))
       (t/is (= nil (f "（括号内")))
       (t/is (= {:token \项 :nth 5 :head "（五）" :text "（五）……"}
                (f "（五）……")))))}
  [line]
  (when-let [[i processed] (括号数字 line)]
    {:token \项 :nth i :head (str/join processed) :text line}))
