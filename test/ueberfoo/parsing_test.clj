(ns ueberfoo.parsing-test
  (:use [ueberfoo.parsing])
  (:use [ueberfoo.common])
  (:use [clojure.test]))

(deftest test-mk-key-from-vc
  (is (= :foo (mk-key-from-vc ["f" "o" "o"])))
  (is (= :foo (mk-key-from-vc [\f \o \o]))))

(deftest test-mk-entry
  (is (true? (map-corresp? {:text "foo bar"}
                           (mk-entry {:text-vc (vec " foo bar ")})
                           :text)))
  (is (true? (map-corresp? {:k "v", :a "b"}
                           (mk-entry {:kv-vc [:k "v" :a "b"]})
                           :k :a))))

(deftest test-add-curr-tag
  (is (true? (map-corresp? {:tags #{:foo}}
                           (add-curr-tag {:tags #{} :curr-k-vc ["f" "o" "o"]})
                           :tags)))
  (is (true? (map-corresp? {:tags #{:foo :bar}}
                           (add-curr-tag {:tags #{:foo} :curr-k-vc ["b" "a" "r"]})
                           :tags))))

(deftest test-init-prs-val
  (is (true? (map-corresp? {:kv-vc [:a "b" :foo], :curr-v-vc []}
                           (init-prs-val {:kv-vc [:a "b"], :curr-k-vc ["f" "o" "o"]})
                           :kv-vc :curr-v-vc))))

(deftest test-add-curr-val
  (is (true? (map-corresp? {:kv-vc [:a "b" :foo "bar"]}
                           (add-curr-val {:kv-vc [:a "b" :foo],
                                          :curr-v-vc ["b" "a" "r"]})
                           :kv-vc))))

(deftest test-conj-to
  (is (= {:k ["f"]}         (conj-to {:k []}        :k "f")))
  (is (= {:k ["f" "o"]}     (conj-to {:k ["f"]}     :k "o")))
  (is (= {:k ["f" "o" "o"]} (conj-to {:k ["f" "o"]} :k "o")))  
  (is (= {:k [1 2 3]}       (conj-to {:k [1 2]}     :k 3))))

(deftest test-parse-entry
  (is (true? (map-corresp? {:text "foo bar"}
                           (parse-entry " foo bar ")
                           :text)))  
  (is (true? (map-corresp? {:tags #{:foo :bar}}
                           (parse-entry " #foo #bar ")
                           :tags)))
  (is (true? (map-corresp? {:link "http://clojure.org" :rating "1"}
                           (parse-entry " #link=http://clojure.org #rating=1")
                           :link :rating)))
  (is (true? (map-corresp? {:text "foo bar"
                            :tags #{:foo :bar}
                            :link "http://clojure.org" :rating "1"}
                           (parse-entry " foo bar #foo #bar #link=http://clojure.org #rating=1")
                           :text :tags :link :rating)))
  (is (true? (map-corresp? {:text "foo bar"
                            :tags #{:foo :bar}
                            :link "http://clojure.org" :rating "1"}
                           (parse-entry "#bar foo #rating=1 #foo #link=http://clojure.org bar")
                           :text :tags :link :rating))))
                         
(deftest test-mk-kv
  (is (= {:foo "bar"} (mk-kv "#foo=bar")))
  (is (= {:link "http://clojure.org"} (mk-kv "#link=http://clojure.org"))))

(deftest test-mk-token
  (is (= {:link "http://clojure.org"} (mk-token "#link=http://clojure.org")))
  (is (= :foo (mk-token "#foo")))
  (is (= "bar" (mk-token "bar"))))

(defn mk-lom [m]  
  "make list options map"
  (merge default-options m))

(deftest test-parse-list-options
  (is (= (mk-lom {:filter ["foo"]})
         (parse-list-options ["-f" "foo"])))
  (is (= (mk-lom {:filter [:bar]})
         (parse-list-options ["-f" "#bar"])))
  (is (= (mk-lom {:filter [{:foo "bar"}]})
         (parse-list-options ["-f" "#foo=bar"])))
  (is (= (mk-lom {:filter ["foo" :bar {:foo "bar"}]})
         (parse-list-options ["-f" "foo" "#bar" "#foo=bar"])))
  (is (= (mk-lom {:select [:foo :bar]})
         (parse-list-options ["-s" "foo" "bar"])))
  (is (= (mk-lom {:all true})
         (parse-list-options ["-*"])))
  (is (= (mk-lom {:reverse true})
         (parse-list-options ["-r"])))  
  (is (= (mk-lom {:limit 3})
         (parse-list-options ["-l" "3"])))
  (is (= (mk-lom {:offset 3})
         (parse-list-options ["-o" "3"])))
  (is (= (mk-lom {:clj true})
         (parse-list-options ["-c"])))
  (is (= (mk-lom {:display "kv"})
         (parse-list-options ["-d" "kv"])))
  (is (= (mk-lom {:verbose true})
         (parse-list-options ["-v"])))
  (is (= (mk-lom {:filter  ["foo" :bar {:foo "bar"}]                  
                  :select  [:foo :bar]
                  :all     true                  
                  :reverse true
                  :limit   3
                  :offset  3
                  :clj     true
                  :display "kv"
                  :verbose true})
         (parse-list-options
          (vec (.split
                (str "-f foo #bar #foo=bar -s foo bar "
                     "-* -r -l 3 -o 3 -c -d kv -v")
                " ")))))
  (is (empty? (non-corresp-keys {:filter  ["foo" :bar {:foo "bar"}]                  
                                 :select  [:foo :bar]
                                 :all     true                  
                                 :reverse true
                                 :limit   3
                                 :offset  3
                                 :clj     true
                                 :display "kv"
                                 :verbose true}
                                (parse-list-options
                                 (vec (.split
                                       (str "-f foo #bar #foo=bar -s foo bar "
                                            "-* -r -l 3 -o 3 -c -d kv -v")
                                       " "))))))
  (is (empty? (non-corresp-keys {:filter  [{:foo "bar"} "foo" :bar]                  
                                 :select  [:foo :bar]
                                 :all     true                  
                                 :reverse true
                                 :limit   3
                                 :offset  3
                                 :clj     true
                                 :display "kv"
                                 :verbose true}
                                (parse-list-options
                                 (vec (.split
                                       (str "-v -* -s foo bar -o 3 -c -d kv -r "
                                            "-l 3 -f #foo=bar foo #bar")
                                        " ")))))))
  
