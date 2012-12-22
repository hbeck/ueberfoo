(ns ueberfoo.common.date-test
  (:use [ueberfoo.common.date])
  (:use [ueberfoo.common.conversion])
  (:import [java.util Date Calendar GregorianCalendar TimeZone Locale])
  (:use [clojure.test]))

(deftest test-year-of
  (is (= 2011 (year-of (.parse sdf-date "2011-03-14"))))
  (is (= 2012 (year-of (.parse sdf-date "2012-03-14"))))
  (is (= 2099 (year-of (.parse sdf-date "2099-03-14")))))

(deftest test-month-of
  (is (= 1  (month-of (.parse sdf-date "2011-01-14"))))
  (is (= 3  (month-of (.parse sdf-date "2011-03-14"))))
  (is (= 12 (month-of (.parse sdf-date "2011-12-14")))))

(deftest test-calendar-field
  (is (= Calendar/DATE (calendar-field :day-of-month)))
  (is (= Calendar/DAY_OF_YEAR (calendar-field :day-of-year)))
  (is (= Calendar/YEAR (calendar-field :year)))
  (is (= Calendar/MONTH (calendar-field :month)))
  (is (= Calendar/HOUR_OF_DAY (calendar-field :hour)))
  (is (= Calendar/MINUTE (calendar-field :minute)))
  (is (= Calendar/SECOND (calendar-field :second)))
  (is (= Calendar/MILLISECOND (calendar-field :millisecond))))
