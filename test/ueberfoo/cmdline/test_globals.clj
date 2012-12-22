(ns ueberfoo.cmdline.test-globals)

(def aima {:id 1
           :link "http://aima.cs.berkeley.edu/"
           :tags #{:ai :book}
           :text "artificial intelligence a modern approach"})

(def sicp {:id 2
           :link "http://mitpress.mit.edu/sicp/full-text/book/book.html"
           :tags #{:scheme :programming :mit :book}
           :text "structure and interpretation of computer programs"})

(def joy-clj {:id 3
              :tags #{:clojure :programming :book}
              :text "the joy of clojure"
              :author "fogus"})

(def entries3 [aima sicp joy-clj])

(def entry3kvs
  {:text "random foo" :link "http" :author "hb" :purpose :test-kv})

