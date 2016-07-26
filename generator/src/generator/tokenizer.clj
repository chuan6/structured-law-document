(ns generator.tokenizer
  (:require [clojure.string :as str]
            [clojure.test :as t]
            [clojure.zip :as z]
            [generator.test :as tt]))

(def cdigit-map
  {\零 0 \一 1 \二 2 \三 3 \四 4 \五 5 \六 6 \七 7 \八 8 \九 9})

(def nthten-map
  {\十 10 \百 100})

(def numchar-zh-set (set (concat (keys cdigit-map) (keys nthten-map))))

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
  (let [[c & cs] line]
    (when (= c \第)
      (let [[head tail] (split-with numchar-zh-set cs)]
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

(defn nth-项
  {:test
   #(let [f nth-项]
      (tt/comprehend-tests
       (t/is (= nil (f "（")))
       (t/is (= nil (f "（括号内")))
       (t/is (= {:token \项 :nth 5 :head "（五）" :text "（五）……"}
                (f "（五）……")))))}
  [line]
  (assert (= (first line) \（))
  (let [[i-zh tail] (split-with (partial not= \）) (rest line))]
    (when (and (seq tail) (every? numchar-zh-set i-zh))
      (when-let [i (chinese-number i-zh)]
        {:token \项 :nth i :head (str/join (-> [\（] (into i-zh) (conj \）)))
         :text line}))))

(def txts ["本法第三十九条和第四十条第一项、第二项"
           "本法第三十九条和第四十条第一项、第二项"
           "本法第二十二条和第二十三条"
           "本法第二十六条第一款"
           "本法第二十六条第一款第一项"
           "本条第一款"
           "本法第四十条、第四十一条"
           "本法第四十二条"
           "本法第四十二条第二项"
           "本法第三十八条"
           "本法第三十六条"
           "本法第四十条"
           "本法第四十一条第一款"
           "本法第四十四条第一项"
           "本法第四十四条第四项、第五项"
           "本法第八十七条"
           "本法第十七条"
           "本法第三十六条、第三十八条"
           "本法第三十九条和第四十条第一项、第二项"
           "本法第二十六条"
           "本法第四十七条"
           "本法第十四条 第二款第三项"
           "本法第四十六条"])

;(clojure.pprint/pprint (map #(str/split % #"[法条款项和、]") txts))

(def item-char-set (set (seq "法条款项本第一二三四五六七八九十百")))

(def separators #{\space \、 \和})

(defn- left-most-item [s]
  (loop [xs []
         ys s]
    (if (item-char-set (first ys))
      (let [xs' (conj xs (first ys))
            ys' (rest ys)]
        (if (#{"法" "条" "款" "项"} (str (first ys)))
          [xs' ys']
          (recur xs' ys')))
      [[] s])))

(defn- read-items [char-seq]
  (loop [[c :as cs] char-seq
         ts []]
    (let [[item rest-cs] (left-most-item cs)]
      (cond
        (separators c)
        (recur (rest cs) (conj ts {:token :separator :text (str c)}))

        (empty? item)
        [ts rest-cs]

        :read-an-item
        (case c
          \本
          (recur rest-cs (conj ts {:token (peek item) :nth :this
                                   :text (str/join item)}))

          \第
          (let [[i unit] (nth-item item)]
            (recur rest-cs (conj ts {:token unit :nth i
                                     :text (str/join item)}))))))))

(def parse-tree (partial z/zipper
                         seq? ;branch?
                         rest ;children
                         (fn [x children] ;makenode
                           (cons (first x) children))))

(defn- parse [cs]
  (let [[recognized-items rest-cs] (read-items cs)]
    (reduce
     (fn [loc x]
       (let [curr-t (:token (if (z/branch? loc)
                            (first (z/node loc))
                            (z/node loc)))
             x-t (:token x)]
         (case [curr-t x-t]
           [\法 \条] (-> loc (z/append-child (list x)) z/down z/rightmost)
           [\条 \款] (-> loc (z/append-child (list x)) z/down z/rightmost)
           [\条 \项] (-> loc (z/append-child (list {:token \款 :nth 1})) z/down z/rightmost
                         (z/append-child x) z/down z/rightmost)
           [\款 \项] (-> loc (z/append-child x) z/down z/rightmost)

           [\条 \条] (recur (-> loc z/up) x)
           [\款 \款] (recur (-> loc z/up) x)
           [\项 \项] (recur (-> loc z/up) x)

           [\款 \条] (recur (-> loc z/up z/up) x)
           [\项 \款] (recur (-> loc z/up z/up) x)
           [\项 \条] (recur (-> loc z/up z/up z/up) x)

           (do (println "unrecognized pattern" [curr-t x])
               loc))))
     (parse-tree (list (first recognized-items)))
     (remove #(= (:token %) :separator) (rest recognized-items)))))

(tt/comprehend-tests
 (for [src txts
       :let [r (parse src)]]
   (do (clojure.pprint/pprint (z/root r))
       (t/is (= (str/join (str/split src #"[ 、和]"))
                (str/join (map :text (flatten (z/root r)))))))))
