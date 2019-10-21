(ns ribelo.torgau
  (:refer-clojure :exclude [list])
  (:require
   [taoensso.encore :as e]
   #?(:clj  [java-time :as jt]
      :cljs [ribelo.oyeblikk :as oy])))

#?(:clj (derive clojure.lang.Keyword ::keyword)
   :cljs (derive cljs.core/Keyword ::keyword))
#?(:clj (derive java.lang.String ::string)
   :cljs (derive js/String ::string))
(derive ::keyword ::header)
(derive ::string ::header)
#?(:clj (derive java.util.Map ::map)
   :cljs (do (derive cljs.core/PersistentArrayMap ::map)
             (derive cljs.core/PersistentHashMap ::map)))
#?(:clj (derive java.util.Collection ::collection)
   :cljs (do (derive cljs.core/PersistentVector ::collection)
             (derive cljs.core/LazySeq ::collection)))

(def class*
  #?(:clj class
     :cljs type))

(defn parse-date [s]
  (or (e/catching
       #?(:clj  (jt/local-date "'['yyyy-MM-dd E']'" s)
          :cljs (oy/parse-iso (re-find #"\d{4}-\d{2}-\d{2}" s))))
      (e/catching
       #?(:clj  (jt/local-date "'<'yyyy-MM-dd E'>'" s)
          :cljs (oy/parse-iso (re-find #"\d{4}-\d{2}-\d{2}" s))))))

(defn parse-datetime [s]
  (or (e/catching
       #?(:clj  (jt/local-date-time "'['yyyy-MM-dd E HH:mm']'" s)
          :cljs (oy/parse (apply str (rest (re-find #"(\d{4}-\d{2}-\d{2}) (?:\w{3}) (\d{2}:\d{2})" s))) "yyyy-MM-ddHH:mm")))
      (e/catching
       #?(:clj  (jt/local-date-time "'<'yyyy-MM-dd E HH:mm'>'" s)
          :cljs (oy/parse (apply str (rest (re-find #"(\d{4}-\d{2}-\d{2}) (?:\w{3}) (\d{2}:\d{2})" s))) "yyyy-MM-ddHH:mm")))))

(defn ->inactive-date [date]
  #?(:clj  (jt/format "'['yyyy-MM-dd E']'" date)
     :cljs (oy/format date "[yyyy-MM-dd E]")))

(defn ->inactive-datetime [datetime]
  #?(:clj  (jt/format "'['yyyy-MM-dd E HH:mm']'" datetime)
     :cljs (oy/format date "[yyyy-MM-dd E HH:mm]")))

(defn ->active-date [date]
  #?(:clj  (jt/format "'<'yyyy-MM-dd E'>'" date)
     :cljs (oy/format date "<yyyy-MM-dd E>")))

(defn ->active-datetime [datetime]
  #?(:clj  (jt/format "'<'yyyy-MM-dd E HH:mm'>'" datetime)
     :cljs (oy/format date "<yyyy-MM-dd E> HH:mm")))

(defn list [seq]
  (doseq [elem seq]
    (println (str "- " elem))))

(defmulti table (fn [[x & _] & [[y & _]]] [(class* x) (class* y)]))

(defmethod table [::header ::map]
  [ks rows]
  (let [widths  (map
                    (fn [k]
                      (apply max (count (str k)) (map #(count (str (get % k))) rows)))
                    ks)
           align   (->> ks
                        (map (fn [k]
                               (if (every? number? (map #(get % k) rows)) :r :l))))
           spacers (map #(apply str (repeat % "-")) widths)
           fmts    (map #(case %2
                           :r (str "%" %1 "s")
                           :l (str "%-" %1 "s")) widths align)
           fmt-row (fn [leader divider trailer row]
                     (str leader
                          (apply str (interpose divider
                                                (for [[col fmt] (map vector (map #(get row %) ks) fmts)]
                                                  (format fmt (str col)))))
                          trailer \newline))]
       (print (fmt-row "| " " | " " |" (zipmap ks ks)))
       (print (fmt-row "|-" "-+-" "-|" (zipmap ks spacers)))
       (doseq [row rows]
         (print (fmt-row "| " " | " " |" row)))))

(defmethod table [::map nil]
  [rows]
  (table (keys (first rows)) rows))
