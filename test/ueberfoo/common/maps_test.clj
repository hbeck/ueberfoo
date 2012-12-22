(ns ueberfoo.common.maps-test
  (:use [ueberfoo.common.maps])
  (:use [clojure.test]))

(deftest test-merge-if
  (is (= {:a :b}        (merge-if false {:a :b} {:k :v})))
  (is (= {:a :b, :k :v} (merge-if true  {:a :b} {:k :v}))))

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
