(ns ueberfoo.cmdline.io-test
  (:use [ueberfoo.cmdline.io])
  (:use [clojure.test]))

(deftest test-mk-entry-path
  (is (= "/tmp/2012/8/id42.clj"
         (mk-entry-path "/tmp" 2012 8 42))))

