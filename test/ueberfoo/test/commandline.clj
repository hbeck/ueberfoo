(ns ueberfoo.test.commandline
  (:use [ueberfoo.commandline])
  (:use [clojure.test]))


(def aima {:link "http://aima.cs.berkeley.edu/"
          :tags #{:ai :book}
          :text "artificial intelligence a modern approach"})

(def sicp {:link "http://mitpress.mit.edu/sicp/full-text/book/book.html"
           :tags #{:scheme :programming :mit :book}
           :text "structure and interpretation of computer programs"})

(def v1 [aima sicp])

(deftest new-entry-tests
  (is (=  aima
          (select-keys
          (make-new-entry ["artificial" "intelligence" "a" "modern" "approach" "#book" "#ai" "#link=http://aima.cs.berkeley.edu/"])
          [:text :tags :link])))
  (is (= sicp
         (select-keys
          (make-new-entry ["structure" "and" "interpretation" "of" "computer" "programs" "#book" "#mit" "#link=http://mitpress.mit.edu/sicp/full-text/book/book.html" "#scheme" "#programming"])
          [:text :tags :link]))))

(deftest list-tests
  (is (= 'todo 'todo)))
