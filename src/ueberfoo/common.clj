;; created 2012-01-16 00:44
;; author Harald Beck

(ns ueberfoo.common)

(def sdf-date-time (java.text.SimpleDateFormat. "yyyy-MM-dd HH:mm:ss"))
(def sdf-date (java.text.SimpleDateFormat. "yyyy-MM-dd"))
(def ueberfoo-format {:name "ueberfoo", :version "0.0.1"})

(defn l-and
  "logical and"
  ([] false)
  ([x] (if x true false))
  ([x y] (and x y))
  ([x y & z] (if x (apply l-and y z) false)))

(defn l-or
  "logical or"
  ([] false)
  ([x] (if x true false))
  ([x y] (or x y))
  ([x y & z] (if x true (apply l-or y z)))) 

(defn has-key?
  ([m k]
     "checks whether map m contains key k at all"
     (not (nil? (k m))))
  ([m k pred]
     "checks whether map m contains key k
      and additionally if its value satisfies predicate pred"
     (let [v (k m)]
       (if (nil? v) false (pred v)))))

(defn make-timestamp-str []
  (.format sdf-date-time (java.util.Date.)))

(defn can-as-number? [x]
  (if (number? x)
    true
    (try
      (do (Long/parseLong x) true)    
      (catch NumberFormatException e false))))

(defn as-number [x]
  (if (number? x)
    x
    (try (Long/parseLong x)
         (catch NumberFormatException e nil))))
     
(defn assoc-if [t m k v]
  "if k is not nil and v is not empty and t is true associates k v in m,
   else returns m"
  (if (or (not t) (nil? k) (empty? v)) m (assoc m k v)))

(defn merge-if [t m1 m2]
  "returns m1 and m2 merged, if t, else m1"
  (if t (merge m1 m2) m1))

(defn tag-str-to-keyword [s]
  (when (.startsWith s "#")
    (keyword (.substring s 1))))

(defn keyword-to-tag-str [k]
  (when (keyword? k)
    (str "#" (name k))))

(defn substring-of? [s1 s2]
  "true iff s1 is a substring of s2"
  (when-not (or (nil? s1) (nil? s2))
    (< -1 (.indexOf s2 s1))))

(defn supstring-of? [s1 s2]
  "true iff s2 is a substring of s1"
  (when-not (or (nil? s1) (nil? s2))
    (< -1 (.indexOf s1 s2))))

