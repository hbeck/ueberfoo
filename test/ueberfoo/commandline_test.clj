(ns ueberfoo.commandline-test
  (:use [ueberfoo.common])
  (:use [ueberfoo.commandline])
  (:use [clojure.test]))

(deftest test-mkfn-list-post-filter
  (let [lpp (mkfn-list-post-filter
             {:nr-of-entries 4, :offset 1, :limit 2})]
    (is (= [2 3] (lpp [1 2 3 4]))))
  (let [lpp (mkfn-list-post-filter
             {:nr-of-entries 4, :offset 1, :limit 2, :reverse true})]
    (is (= [3 2] (lpp [1 2 3 4]))))
  (let [lpp (mkfn-list-post-filter
             {:nr-of-entries 4, :offset 2, :limit 1, :reverse true})]
    (is (= [2] (lpp [1 2 3 4]))))
  (let [lpp (mkfn-list-post-filter
             {:nr-of-entries 4, :offset 0, :limit 3, :reverse true})]
    (is (= [4 3 2] (lpp [1 2 3 4])))))

(deftest test-string-repr
  (is (= "foo" (string-repr "foo")))
  (is (= "#foo" (string-repr :foo)))
  (is (= "foo #bar" (string-repr ["foo" :bar]))))

(deftest test-mkfn-kv-formatter  
  (is (= "a: b"          ((mkfn-kv-formatter "kv") ["a" "b"])))
  (is (= "#a: #b"        ((mkfn-kv-formatter "kv") [:a :b])))
  (is (= "foo: bar"      ((mkfn-kv-formatter "kv") ["foo" "bar"])))
  (is (= "#foo: #bar"    ((mkfn-kv-formatter "kv") [:foo :bar])))
  (is (= "foo: bar #xyz" ((mkfn-kv-formatter "kv") ["foo" ["bar" :xyz]])))  
  (is (= "a"    ((mkfn-kv-formatter "k") ["a" "b"])))
  (is (= "#a"   ((mkfn-kv-formatter "k") [:a :b])))
  (is (= "foo"  ((mkfn-kv-formatter "k") ["foo" "bar"])))
  (is (= "#foo" ((mkfn-kv-formatter "k") [:foo :bar])))
  (is (= "foo"  ((mkfn-kv-formatter "k") ["foo" ["bar" :xyz]])))  
  (is (= "b"        ((mkfn-kv-formatter "v") ["a" "b"])))
  (is (= "#b"       ((mkfn-kv-formatter "v") [:a :b])))
  (is (= "bar"      ((mkfn-kv-formatter "v") ["foo" "bar"])))
  (is (= "#bar"     ((mkfn-kv-formatter "v") [:foo :bar])))
  (is (= "bar #xyz" ((mkfn-kv-formatter "v") ["foo" ["bar" :xyz]]))))

(def aima {:link "http://aima.cs.berkeley.edu/"
           :tags #{:ai :book}
           :text "artificial intelligence a modern approach"})

(def sicp {:link "http://mitpress.mit.edu/sicp/full-text/book/book.html"
           :tags #{:scheme :programming :mit :book}
           :text "structure and interpretation of computer programs"})

(def joy-clj {:tags #{:clojure :programming :book}
              :text "the joy of clojure"
              :author "fogus"})

(def entries3 [aima sicp joy-clj])

(deftest test-mkfn-filter
  (is (= #{aima}
         (set (filter (mkfn-filter {:filter ["art"]}) entries3))))
  (is (= #{aima sicp joy-clj}         
         (set (filter (mkfn-filter {:filter [:book]}) entries3))))
  (is (= #{sicp joy-clj}
         (set (filter (mkfn-filter {:filter [:programming]}) entries3))))
  (is (= #{joy-clj}
         (set (filter (mkfn-filter {:filter [{:author "fogus"}]}) entries3))))
  (is (= #{joy-clj}
         (set (filter (mkfn-filter {:filter [{:author "g"}]}) entries3))))
  (is (= #{joy-clj}
         (set (filter (mkfn-filter {:filter [{:author :*}]}) entries3))))
  (is (= #{aima sicp}
         (set (filter (mkfn-filter {:filter [{:link :*}]}) entries3))))
  (is (= #{aima sicp}
         (set (filter (mkfn-filter {:filter [{:link "http"}]}) entries3))))
  (is (= #{aima sicp}
         (set (filter (mkfn-filter {:filter [{:link "edu"}]}) entries3))))
  (is (= #{aima}
         (set (filter (mkfn-filter {:filter [{:link "aima"}]}) entries3))))
  (is (= #{aima sicp}
         (set (filter (mkfn-filter {:filter [:book {:link "edu"}]}) entries3))))
  (is (= #{aima sicp joy-clj}
         (set (filter (mkfn-filter {:filter [:book "o"]}) entries3))))
  (is (= #{aima sicp}
         (set (filter (mkfn-filter {:filter [:book "a"]}) entries3))))
  (is (= #{aima sicp joy-clj}
         (set (filter (mkfn-filter {:filter [:book "t"]}) entries3))))
  (is (= #{aima sicp joy-clj}
         (set (filter (mkfn-filter {:filter [:book "t" "o"]}) entries3))))
  (is (= #{aima sicp joy-clj}
         (set (filter (mkfn-filter {:filter ["t" "o"]}) entries3))))
  (is (= #{sicp joy-clj}
         (set (filter (mkfn-filter {:filter [:programming "t" "o"]}) entries3))))
  (is (= #{joy-clj}
         (set (filter (mkfn-filter {:filter [:programming "t" "o" {:author :*}]}) entries3))))
  (is (= #{}
         (set (filter (mkfn-filter {:filter [:programming "t" "o" {:author "dilbert"}]}) entries3))))
  (is (= #{}
         (set (filter (mkfn-filter {:filter ["aima" {:author :*}]}) entries3))))
  (is (= #{}
         (set (filter (mkfn-filter {:filter [{:link :*} "joy"]}) entries3))))
  (is (= #{}
         (set (filter (mkfn-filter {:filter ["art" :scheme]}) entries3)))))

(deftest test-matches-text?
  (is (true?  (matches-all-text? aima ["artificial"])))
  (is (true?  (matches-any-text? aima ["artificial"])))
  (is (true?  (matches-all-text? aima ["artificial" "intelligence"])))
  (is (true?  (matches-any-text? aima ["artificial" "intelligence"])))
  (is (false? (matches-all-text? aima ["artificial" "intelligence" "book"])))
  (is (true?  (matches-any-text? aima ["artificial" "intelligence" "book"])))
  (is (true?  (matches-all-text? aima [])))
  (is (false? (matches-any-text? aima []))))

(deftest test-matches-tags?
  (is (true?  (matches-all-tags? sicp [:scheme])))
  (is (true?  (matches-any-tags? sicp [:scheme])))
  (is (false? (matches-all-tags? sicp [:fail])))
  (is (false? (matches-any-tags? sicp [:fail])))
  (is (true?  (matches-all-tags? sicp [:scheme :programming :book])))
  (is (true?  (matches-any-tags? sicp [:scheme :programming :book])))
  (is (false? (matches-all-tags? sicp [:scheme :programming :fail])))
  (is (true?  (matches-any-tags? sicp [:scheme :programming :fail])))
  (is (true?  (matches-any-tags? sicp [:fail :programming])))
  (is (true?  (matches-all-tags? sicp [])))
  (is (false? (matches-any-tags? sicp [:fail]))))

(def entry3kvs
  {:text "random foo" :link "http" :author "hb" :purpose :test-kv})

(deftest test-matches-all-kvs?
  (is (true?  (matches-all-kvs? entry3kvs [{:link "http"}])))
  (is (true?  (matches-all-kvs? entry3kvs [{:link "http"} {:author "hb"}])))
  (is (true?  (matches-all-kvs? entry3kvs [{:link "http"} {:author "hb"} {:purpose :test-kv}])))
  (is (true?  (matches-all-kvs? entry3kvs [{:purpose :test-kv} {:author "hb"}])))
  (is (true?  (matches-all-kvs? entry3kvs [{:purpose :test-kv} {:author "hb"}])))
  (is (true?  (matches-all-kvs? entry3kvs [{:purpose :test-kv}])))
  (is (true?  (matches-all-kvs? entry3kvs [{:link "tt"}])))
  (is (true?  (matches-all-kvs? entry3kvs [{:link :*}])))
  (is (true?  (matches-all-kvs? entry3kvs [{:link "tt"} {:author "hb"}])))
  (is (true?  (matches-all-kvs? entry3kvs [{:link :*} {:author "hb"}])))
  (is (true?  (matches-all-kvs? entry3kvs [{:link :*} {:author "h"}])))
  (is (true?  (matches-all-kvs? entry3kvs [{:link :*} {:author :*}])))
  (is (false? (matches-all-kvs? entry3kvs [{:link "fail"}])))
  (is (false? (matches-all-kvs? entry3kvs [{:link :*} {:fail :*}])))
  (is (false? (matches-all-kvs? entry3kvs [{:link :*} {:fail "foo"}])))
  (is (false? (matches-all-kvs? entry3kvs [{:fail :*}])))
  (is (false? (matches-all-kvs? entry3kvs [{:link "t"} {:author "fail"}])))
  (is (false? (matches-all-kvs? entry3kvs [{:link :*} {:author "fail"}])))
  (is (true?  (matches-all-kvs? entry3kvs []))))
  

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

