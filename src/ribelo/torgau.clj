(ns ribelo.torgau
  (:require
   [taoensso.encore :as e]
   [java-time :as jt]
   [clojure.pprint :as pp]))

(defn parse-date [s]
  (or (e/catching
       (jt/local-date "'['yyyy-MM-dd E']'" s))
      (e/catching
       (jt/local-date "'<'yyyy-MM-dd E'>'" s))))

(defn parse-datetime [s]
  (or (e/catching
       (jt/local-date-time "'['yyyy-MM-dd E HH:mm']'" s))
      (e/catching
       (jt/local-date-time "'<'yyyy-MM-dd E HH:mm'>'" s))))

(defn ->inactive-date [date]
  (jt/format "'['yyyy-MM-dd E']'" date))

(defn ->inactive-datetime [datetime]
  (jt/format "'['yyyy-MM-dd E HH:mm']'" datetime))

(defn ->active-date [date]
  (jt/format "'<'yyyy-MM-dd E'>'" date))

(defn ->active-datetime [datetime]
  (jt/format "'<'yyyy-MM-dd E HH:mm'>'" datetime))

(defn table
  ([ks rows]
   (when (seq rows)
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
       (apply str
              (into [(fmt-row "| " " | " " |" (zipmap ks ks))
                     (fmt-row "|-" "-+-" "-|" (zipmap ks spacers))]
                    (for [row rows]
                      (fmt-row "| " " | " " |" row)))))))
  ([rows] (table (keys (first rows)) rows)))
