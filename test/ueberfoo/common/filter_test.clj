(ns ueberfoo.common.filter_test
  (:use [ueberfoo.common.filter])
  (:use [ueberfoo.cmdline.test-globals])
  (:use [clojure.test]))

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
