(ns ueberfoo.cmdline.transform-test
  (:use [ueberfoo.cmdline.transform])
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
