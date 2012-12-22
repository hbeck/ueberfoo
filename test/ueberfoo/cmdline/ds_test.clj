(ns ueberfoo.cmdline.ds-test
  (:use [ueberfoo.cmdline.ds])
  (:use [ueberfoo.common.maps])
  (:use [ueberfoo.cmdline.test-globals])
  (:use [clojure.test]))

(deftest test-mk-new-entry
  (is (map-corresp?
       aima
       (mk-new-entry ["artificial intelligence"
                      "a modern approach"
                      "#ai #book"
                      "#link=http://aima.cs.berkeley.edu/"])
       :text :tags :link))
  (is (map-corresp?
       sicp
       (mk-new-entry ["structure and interpretation"
                      "of computer programs"
                      "#scheme #programming" "#mit" "#book"
                      "#link=http://mitpress.mit.edu/sicp/full-text/book/book.html"])
       :text :tags :link))
  (is (map-corresp?
       joy-clj
       (mk-new-entry ["the joy of clojure #author=fogus #clojure #programming #book"])
       :text :tags :author))
  (is (map-corresp? {:text "asdf"}    (mk-new-entry ["asdf"]) :text))
  (is (map-corresp? {:text "foo bar"} (mk-new-entry ["foo bar"]) :text))
  (is (map-corresp? {:text "foo bar"} (mk-new-entry ["foo" "bar"]) :text))
  (is (map-corresp? {:text "asdf", :tags #{:foo :bar}}
                    (mk-new-entry ["asdf #foo #bar"])
                    :text :tags))
  (is (map-corresp? {:text "asdf", :tags #{:foo :bar}}
                    (mk-new-entry [" asdf #foo #bar" ])
                    :text :tags))
  (is (map-corresp? {:text "asdf", :tags #{:foo :bar}}
                    (mk-new-entry ["asdf" "#foo #bar"])
                    :text :tags))
  (is (map-corresp? {:text "asdf", :tags #{:foo :bar}}
                    (mk-new-entry ["asdf" "#foo" "#bar"])
                    :text :tags))
  (is (map-corresp? {:text "asdf", :tags #{:foo :bar}}
                    (mk-new-entry ["#foo" "asdf" "#bar"])
                    :text :tags))
  (is (map-corresp? {:text "asdf", :tags #{:foo :bar}}
                    (mk-new-entry ["#foo" "#bar" "asdf"])
                    :text :tags))
  (is (map-corresp? {:text "asdf", :tags #{:foo :bar}, :link "http://clojure.org"}
                    (mk-new-entry ["#link=http://clojure.org" "#foo" "asdf" "#bar"])
                    :text :tags :link))
  (is (map-corresp? {:text "asdf", :tags #{:foo :bar}, :as "df", :gh :ij}
                    (mk-new-entry ["#foo" "asdf" "#bar" "#as=df" "#gh=#ij"])
                    :text :tags)))