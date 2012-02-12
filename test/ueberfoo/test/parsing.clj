(ns ueberfoo.test.parsing
  (:use [ueberfoo.parsing])
  (:use [clojure.test]))

(deftest opt-map-generation
  (is (= {:select [:text], :display "v"}
         (parse-list-options []))))
