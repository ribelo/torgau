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

(defn org-table
  ([ks rows]
     (when (seq rows)
       (let [widths (map
                     (fn [k]
                       (apply max (count (str k)) (map #(count (str (get % k))) rows)))
                     ks)
             spacers (map #(apply str (repeat % "-")) widths)
             fmts (map #(str "%" % "s") widths)
             fmt-row (fn [leader divider trailer row]
                       (str leader
                            (apply str (interpose divider
                                                  (for [[col fmt] (map vector (map #(get row %) ks) fmts)]
                                                    (format fmt (str col)))))
                            trailer))]
         (println (fmt-row "| " " | " " |" (zipmap ks ks)))
         (println (fmt-row "|-" "-+-" "-|" (zipmap ks spacers)))
         (doseq [row rows]
           (println (fmt-row "| " " | " " |" row))))))
  ([rows] (org-table (keys (first rows)) rows)))
