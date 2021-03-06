(ns ueberfoo.common.common-test
  (:use [ueberfoo.common common])
  (:use [clojure.test]))

(deftest test-l-and
  (is (= (l-and []) true))
  (is (= (l-and nil) false))
  (is (= (l-and true) true))
  (is (= (l-and true true) true))
  (is (= (l-and true true true) true))
  (is (= (l-and false) false))
  (is (= (l-and false false) false))
  (is (= (l-and false true) false))
  (is (= (l-and true false) false))
  (is (= (l-and false true true) false))
  (is (= (l-and true false true) false))
  (is (= (l-and true true false) false)))

(deftest test-l-or
  (is (= (l-or []) true))
  (is (= (l-or nil) false))
  (is (= (l-or true) true))
  (is (= (l-or true true) true))
  (is (= (l-or true true true) true))
  (is (= (l-or false) false))
  (is (= (l-or false false) false))
  (is (= (l-or false true) true))
  (is (= (l-or true false) true))
  (is (= (l-or false true true) true))
  (is (= (l-or true false true) true))
  (is (= (l-or true true false) true))
  (is (= (l-or true false false) true))
  (is (= (l-or false true false) true))
  (is (= (l-or false false true) true)))

(deftest test-contained-in?
  (is (true?  (contained-in? :a [:a :b])))
  (is (true?  (contained-in? :b [:a :b])))
  (is (false? (contained-in? :c [:a :b])))
  (is (false? (contained-in? :c [])))
  (is (false? (contained-in? :c nil)))
  (is (true?  (contained-in? :a '(:a :b))))
  (is (true?  (contained-in? :b '(:a :b))))
  (is (false? (contained-in? :c '(:a :b))))
  (is (false? (contained-in? :c '())))
  (is (true?  (contained-in? :a #{:a :b})))
  (is (true?  (contained-in? :a #{:a :b})))
  (is (false? (contained-in? :c #{:a :b})))
  (is (false? (contained-in? :c #{}))))
