;; created 2012-11-26

(ns ueberfoo.common.date
  (:use [ueberfoo.common.conversion])
  (:import [java.util Date Calendar GregorianCalendar TimeZone Locale]))

(def sdf-date-time (java.text.SimpleDateFormat. "yyyy-MM-dd HH:mm:ss"))
(def sdf-date (java.text.SimpleDateFormat. "yyyy-MM-dd" Locale/GERMANY))

(defn mk-cal []
  (let [tz  (TimeZone/getDefault)
        cal (GregorianCalendar. tz)]
    (do (.setTime cal (Date.))
        cal)))

(defn year-of [^Date date]
  (let [cal (Calendar/getInstance)]
    (do
      (.setTime cal date)
      (.get cal (Calendar/YEAR)))))

(defn month-of [^Date date]
  (let [cal (Calendar/getInstance)]
    (do
      (.setTime cal date)
      (inc (.get cal (Calendar/MONTH))))))

(defn timestamp-str []
  (.format sdf-date-time (java.util.Date.)))

(defn calendar-field [k]
  (case k
        :day-of-month Calendar/DATE
        :day-of-year  Calendar/DAY_OF_YEAR
        :year         Calendar/YEAR
        :month        Calendar/MONTH
        :hour         Calendar/HOUR
        :minute       Calendar/MINUTE
        :second       Calendar/SECOND
        :millisecond  Calendar/MILLISECOND
        nil))

; test
(defn date-set
  ([^Date date k v]
     (let [cal (Calendar/getInstance)]
       (do
         (.setTime cal date)
         (.set cal (calendar-field k) v)
         (.getTime cal))))
  ([^Date date k v & args]
     (apply date-set (date-set date k v) args)))

; test
(defn date-add [^Date date k v]
  (let [cal (Calendar/getInstance)]
    (do
      (.setTime cal date)
      (.add cal (calendar-field k) v)
      (.getTime cal))))  

(defn start-of-today [gmt-offset]
  "today's date at 00:00 o'clock"
  (let [cal (Calendar/getInstance)]
    (date-set (.getTime cal)
              :hour gmt-offset
              :minute 0
              :second 0
              :millisecond 0)))

; test
(defn start-of-yesterday [gmt-offset]
  "yesterday's date at 00:00 o'clock"
  (let [t (start-of-today gmt-offset)]
    (date-add t :day-of-year -1)))

; test
(defn parse-date [x]
  "yyyy-MM-dd, 'today', or 'yesterday'"
  (let [c (.charAt x 0)]
    (cond
     (can-char-as-integer? c) (.parse sdf-date x)
     (= "today" x) (start-of-today 1)
     (= "yesterday" x) (start-of-yesterday 1)
     :else nil)))

