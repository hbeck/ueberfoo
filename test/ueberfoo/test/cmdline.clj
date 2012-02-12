(ns ueberfoo.test.cmdline
  (:use [ueberfoo.cmdline])
  (:use [clojure.test]))

(deftest new-entry-tests
  (is (= (select-keys
          (make-new-entry ["artificial" "intelligence" "a" "modern" "approach" "#book" "#ai" "#link=http://aima.cs.berkeley.edu/"])
          [:text :tags :link])
         {:link "http://aima.cs.berkeley.edu/"
          :tags #{:ai :book}
          :text "artificial intelligence a modern approach"})))
