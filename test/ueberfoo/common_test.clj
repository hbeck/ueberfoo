(ns ueberfoo.common-test
  (:use [ueberfoo.common])
  (:use [clojure.test]))

(deftest test-year-of
  (is (= 2011 (year-of (.parse sdf-date "2011-03-14"))))
  (is (= 2012 (year-of (.parse sdf-date "2012-03-14"))))
  (is (= 2099 (year-of (.parse sdf-date "2099-03-14")))))

(deftest test-month-of
  (is (= 1  (month-of (.parse sdf-date "2011-01-14"))))
  (is (= 3  (month-of (.parse sdf-date "2011-03-14"))))
  (is (= 12 (month-of (.parse sdf-date "2011-12-14")))))

(deftest test-l-and
  (is (= (l-and []) true))
  (is (= (l-and nil) false))
  (is (= (l-and true) true))
  (is (= (l-and true true) true))
  (is (= (l-and true true true) true))
  (is (= (l-and false) false))
  (is (= (l-and false false) false))
  (is (= (l-and false true) false))
  (is (= (l-and true false) false))
  (is (= (l-and false true true) false))
  (is (= (l-and true false true) false))
  (is (= (l-and true true false) false)))

(deftest test-l-or
  (is (= (l-or []) true))
  (is (= (l-or nil) false))
  (is (= (l-or true) true))
  (is (= (l-or true true) true))
  (is (= (l-or true true true) true))
  (is (= (l-or false) false))
  (is (= (l-or false false) false))
  (is (= (l-or false true) true))
  (is (= (l-or true false) true))
  (is (= (l-or false true true) true))
  (is (= (l-or true false true) true))
  (is (= (l-or true true false) true))
  (is (= (l-or true false false) true))
  (is (= (l-or false true false) true))
  (is (= (l-or false false true) true)))

(deftest test-can-str-as-integer?
  (is (true?  (can-str-as-integer? "-1")))
  (is (true?  (can-str-as-integer? "0")))
  (is (true?  (can-str-as-integer? "1")))
  (is (false? (can-str-as-integer? "-1.5")))
  (is (false? (can-str-as-integer? "0.5")))
  (is (false? (can-str-as-integer? "1.5")))
  (is (false? (can-str-as-integer? "-1.0")))
  (is (false? (can-str-as-integer? "0.0")))
  (is (false? (can-str-as-integer? "1.0"))))

(deftest test-str-as-integer?
  (is (= -1  (str-as-integer "-1")))
  (is (=  0  (str-as-integer "0")))
  (is (=  1  (str-as-integer "1")))
  (is (= nil (str-as-integer "0.0")))
  (is (= nil (str-as-integer "a"))))

(deftest test-merge-if
  (is (= {:a :b}        (merge-if false {:a :b} {:k :v})))
  (is (= {:a :b, :k :v} (merge-if true  {:a :b} {:k :v}))))

(deftest test-keyword-from-tag-str
  (is (= :asdf (keyword-from-tag-str "#asdf")))
  (is (= nil   (keyword-from-tag-str "asdf")))
  (is (= nil   (keyword-from-tag-str ":asdf"))))

(deftest test-tag-str-from-keyword
  (is (= "#asdf" (tag-str-from-keyword :asdf)))
  (is (= nil     (tag-str-from-keyword ":asdf")))
  (is (= nil     (tag-str-from-keyword "#asdf"))))

(deftest test-tag-str-vs-keyword
  (is (= "#asdf" (tag-str-from-keyword (keyword-from-tag-str "#asdf"))))
  (is (= :asdf   (keyword-from-tag-str (tag-str-from-keyword :asdf)))))

(deftest test-substring-of?
  (is (true?  (substring-of? "" "")))
  (is (true?  (substring-of? "a" "a")))
  (is (true?  (substring-of? "a" "aa")))
  (is (true?  (substring-of? "a" "ab")))
  (is (true?  (substring-of? "a" "ba")))
  (is (true?  (substring-of? "a" "bab")))
  (is (true?  (substring-of? "cde" "abcdeba")))
  (is (true?  (substring-of? "ab" "abcdeba")))
  (is (true?  (substring-of? "ba" "abcdeba")))
  (is (true?  (substring-of? "ello, W" "hello, World!")))
  (is (true?  (substring-of? ".." "...")))
  (is (false? (substring-of? "aa" "a")))
  (is (false? (substring-of? "ab" "a")))
  (is (false? (substring-of? "ba" "a")))
  (is (false? (substring-of? "bab" "a")))
  (is (false? (substring-of? "abcdeba" "cde")))
  (is (false? (substring-of? "abcdeba" "ab")))
  (is (false? (substring-of? "abcdeba" "ba")))
  (is (false? (substring-of? "hello, World!" "ello, W")))
  (is (false? (substring-of? "..." "..")))
  (is (nil?   (substring-of? nil "asdf")))
  (is (nil?   (substring-of? "asdf" nil)))
  (is (nil?   (substring-of? nil nil))))
  

(deftest test-supstring-of?
  (is (true?  (supstring-of? "" "")))
  (is (true?  (supstring-of? "a" "a")))
  (is (false? (supstring-of? "a" "aa")))
  (is (false? (supstring-of? "a" "ab")))
  (is (false? (supstring-of? "a" "ba")))
  (is (false? (supstring-of? "a" "bab")))
  (is (false? (supstring-of? "cde" "abcdeba")))
  (is (false? (supstring-of? "ab" "abcdeba")))
  (is (false? (supstring-of? "ba" "abcdeba")))
  (is (false? (supstring-of? "ello, W" "hello, World!")))
  (is (false? (supstring-of? ".." "...")))
  (is (true?  (supstring-of? "aa" "a")))
  (is (true?  (supstring-of? "ab" "a")))
  (is (true?  (supstring-of? "ba" "a")))
  (is (true?  (supstring-of? "bab" "a")))
  (is (true?  (supstring-of? "abcdeba" "cde")))
  (is (true?  (supstring-of? "abcdeba" "ab")))
  (is (true?  (supstring-of? "abcdeba" "ba")))
  (is (true?  (supstring-of? "hello, World!" "ello, W")))
  (is (true?  (supstring-of? "..." "..")))
  (is (nil?   (supstring-of? nil "asdf")))
  (is (nil?   (supstring-of? "asdf" nil)))
  (is (nil?   (supstring-of? nil nil))))

(deftest test-assoc-if
  (is (= {}             (assoc-if false {} :k :v)))
  (is (= {:k :v}        (assoc-if true  {} :k :v)))
  (is (= {:a :b}        (assoc-if false {:a :b} :k :v)))
  (is (= {:a :b, :k :v} (assoc-if true  {:a  :b} :k :v))))
  
(deftest test-assoc-with
  (is (= {:a :b}       (assoc-with {:a :b}      :a identity)))
  (is (= {:a 2}        (assoc-with {:a 1}       :a inc)))
  (is (= {:a nil}      (assoc-with {:a []}      :a (fn [x] nil))))
  (is (= {:a [0]}      (assoc-with {:a []}      :a #(conj % 0))))
  (is (= {:a [0 1]}    (assoc-with {:a [0]}     :a #(conj % 1))))
  (is (= {:a '(2 0 1)} (assoc-with {:a '(0 1)}  :a #(conj % 2))))
  (is (= {:a '(2 0 1)} (assoc-with {:a '(0 1)}  :a #(cons 2 %))))
  (is (= {:a '(2 0 1)} (assoc-with {:a [0 1]}   :a #(cons 2 %))))
  (is (= {:a {:k 1}}   (assoc-with {:a {:k 0}}  :a (fn [m] (assoc-with m :k inc)))))
  (is (= {:a 2, :b 0}  (assoc-with {:a 1, :b 1} :a inc :b dec))))

(deftest test-contained-in?
  (is (true?  (contained-in? :a [:a :b])))
  (is (true?  (contained-in? :b [:a :b])))
  (is (false? (contained-in? :c [:a :b])))
  (is (false? (contained-in? :c [])))
  (is (false? (contained-in? :c nil)))
  (is (true?  (contained-in? :a '(:a :b))))
  (is (true?  (contained-in? :b '(:a :b))))
  (is (false? (contained-in? :c '(:a :b))))
  (is (false? (contained-in? :c '())))
  (is (true?  (contained-in? :a #{:a :b})))
  (is (true?  (contained-in? :a #{:a :b})))
  (is (false? (contained-in? :c #{:a :b})))
  (is (false? (contained-in? :c #{}))))

(deftest test-map-corresp?
  (is (true? (map-corresp? {:k "v", :a "b"}      {:k "v", :a "b"})))
  (is (true? (map-corresp? {:k "v", :a "b"}      {:k "v", :a "b"} :k)))
  (is (true? (map-corresp? {:k "v", :a "b"}      {:k "v", :a "b"} :k :a)))
  (is (true? (map-corresp? {:k "v", :a "b", 1 2} {:k "v", :a "b"} :k :a)))
  (is (true? (map-corresp? {:k "v", :a "b"}      {:k "v", :a "b", 3 4} :k :a)))
  (is (true? (map-corresp? {:k "v", :a "b", 1 2} {:k "v", :a "b", 3 4} :k :a)))
  (is (true? (map-corresp? {:k "v", :a "b"}      {:k "v", :a "b"} :a)))
  (is (true? (map-corresp? {:k "v", :a "b"}      {:k "v", :a nil} :k)))
  (is (true? (map-corresp? {:k "v", :a "b"}      {:k nil, :a "b"} :a))))

(deftest test-non-corresp-keys
  (is (= [:k]    (non-corresp-keys {:k :v} {:k :w})))
  (is (= []      (non-corresp-keys {:k :w} {:k :w})))
  (is (= []      (non-corresp-keys {:k :v} {:k :w} :k)))
  (is (= []      (non-corresp-keys {:k :v} {:k :v} :l)))
  (is (= []      (non-corresp-keys {:k :v} {:k :v} :k :l)))
  (is (= (set [:k :a])
         (set (non-corresp-keys {:k :v, :a :b} {:k :w}))))
  (is (= [:k]    (non-corresp-keys {:k :v, :a :b} {:k :w} :a)))
  (is (= [:k]    (non-corresp-keys {:k :v, :a :b} {:k :w, :a :b})))
  (is (= []      (non-corresp-keys {:k :v, :a :b} {:k :w, :a :b} :k)))
  (is (= [:k]    (non-corresp-keys {:k :v, :a :b} {:k :w, :a :b} :a)))
  (is (= []      (non-corresp-keys {:k :v, :a :b} {:k :w, :a :b} :a :k)))
  (is (= []      (non-corresp-keys {:k :v, :a :b} {:k :w, :a :b} :a :k :l))))

  

