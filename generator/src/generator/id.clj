(ns generator.id
  (:require [clj-http.util :as http]
            [clojure.string :as str]
            [clojure.test :as t]
            [generator.test :as tt]
            [generator.item-string :as its]))

(declare nth-str)

(defn encode-id [s]
  (str/replace (http/url-encode s) #"%" "."))

(defn entry-id [context t]
  (letfn [(gen-str [context t]
            (let [r (when-let [i (t context)]
                      ;; produce string "lazily"
                      (str (name t) (nth-str i)))]
              (case t
                :编 r
                :则 r
                :章 (str (gen-str context :编) r)
                :节 (str (gen-str context :章) r)
                :条 r
                :款 (str (gen-str context :条) r)
                :项 (str (gen-str context :款) r)
                :目 (str (gen-str context :项) r)
                :序言 "the-preface")))]
    (encode-id (gen-str context t))))

(defn nth-str
  {:test
   #(let [f nth-str]
      (tt/comprehend-tests
       (t/is (= "1" (f 1)))
       (t/is (= "1.001" (f 1001/1000)))))}
  [x]
  (cond (ratio? x)
        (format "%.3f" (float x))

        :else
        (str x)))

(def templates
  {:目 [:条 :款 :项]
   :项 [:条 :款]
   :款 [:条]
   :条 []
   :节 [:编 :章]
   :章 [:编]
   :编 []})

(defn- interpret-nth-with [context {ty :token nx :nth}]
  (if (number? nx)
    nx
    (do (assert (#{:this :prev} nx))
        (({:this identity
           :prev dec
           } nx)
         (ty context)))))

(defn generate
  {:test
   #(let [f generate]
      (tt/comprehend-tests
       (t/is (nil?    (f {} [])))
       (t/is (= "条1" (f {} [{:token :法 :nth :this}
                             {:token :条 :nth 1}])))
       (t/is (= "条1" (f {:条 1} [{:token :条 :nth :this}])))
       (t/is (= "条1款2项3"
                (f {} [{:token :法 :nth :this}
                       {:token :条 :nth 1}
                       {:token :款 :nth 2}
                       {:token :项 :nth 3}])))
       (t/is (= "条1款1项5"
                (f {:条 1 :款 2} [{:token :款 :nth :prev}
                                  {:token :项 :nth 5}])))
       (t/is (= "章2" (f {} [{:token :章 :nth 2}])))
       (t/is (= "章2节1"
                (f {} [{:token :法 :nth :this}
                       {:token :章 :nth 2}
                       {:token :节 :nth 1}])))))}
  ([context ts]
   (let [ty (:token (last ts))]
     (when (contains? templates ty)
       (let [selected-ts (filter (comp not
                                       its/item-types-2
                                       :token)
                                 ts)]
         (generate context (vec selected-ts) ty)))))

  ([context tv expect]
   (let [interpret-nth (partial interpret-nth-with context)]
    (if (nil? expect)
      ""
      (let [{ty :token :as t} (peek tv)
            next-expect (peek (expect templates))]
        (cond (nil? t)
              ;; extend if the entry is available in the context
              ;; otherwise, skip
              (cond-> (generate context [] next-expect)
                (expect context) (str (name expect) (expect context)))

              (= ty expect)
              (str (generate context (pop tv) next-expect)
                   (its/item-type-str ty) (interpret-nth t))

              :mismatch
              (throw (Exception. (str "Expect " expect ", instead "
                                      "get " ty)))))))))
