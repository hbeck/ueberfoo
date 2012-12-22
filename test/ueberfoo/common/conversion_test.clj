(ns ueberfoo.common.conversion-test
  (:use [ueberfoo.common.conversion])
  (:use [clojure.test]))

(deftest test-can-char-as-integer?
  (is (true?  (can-char-as-integer? \1)))
  (is (true?  (can-char-as-integer? \0)))
  (is (false? (can-char-as-integer? \a)))
  (is (false? (can-char-as-integer? \ ))))

(deftest test-can-str-as-integer?
  (is (true?  (can-str-as-integer? "-1")))
  (is (true?  (can-str-as-integer? "0")))
  (is (true?  (can-str-as-integer? "1")))
  (is (false? (can-str-as-integer? "-1.5")))
  (is (false? (can-str-as-integer? "0.5")))
  (is (false? (can-str-as-integer? "1.5")))
  (is (false? (can-str-as-integer? "-1.0")))
  (is (false? (can-str-as-integer? "0.0")))
  (is (false? (can-str-as-integer? "1.0"))))

(deftest test-str-as-integer?
  (is (= -1  (str-as-integer "-1")))
  (is (=  0  (str-as-integer "0")))
  (is (=  1  (str-as-integer "1")))
  (is (= nil (str-as-integer "0.0")))
  (is (= nil (str-as-integer "a"))))

(deftest test-keyword-from-tag-str
  (is (= :asdf (keyword-from-tag-str "#asdf")))
  (is (= nil   (keyword-from-tag-str "asdf")))
  (is (= nil   (keyword-from-tag-str ":asdf"))))

(deftest test-tag-str-from-keyword
  (is (= "#asdf" (tag-str-from-keyword :asdf)))
  (is (= nil     (tag-str-from-keyword ":asdf")))
  (is (= nil     (tag-str-from-keyword "#asdf"))))

(deftest test-tag-str-vs-keyword
  (is (= "#asdf" (tag-str-from-keyword (keyword-from-tag-str "#asdf"))))
  (is (= :asdf   (keyword-from-tag-str (tag-str-from-keyword :asdf)))))
