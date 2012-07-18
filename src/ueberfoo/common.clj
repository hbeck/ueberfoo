;; created 2012-01-16

(ns ueberfoo.common)

(def sdf-date-time (java.text.SimpleDateFormat. "yyyy-MM-dd HH:mm:ss"))
(def sdf-date (java.text.SimpleDateFormat. "yyyy-MM-dd"))

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

(defn timestamp-str []
  (.format sdf-date-time (java.util.Date.)))

(defn can-str-as-integer? [s]
  {:pre [(string? s)]}
  (try
    (do (Integer/parseInt s) true)
    (catch NumberFormatException e false)))

(defn str-as-integer [s]
  {:pre [(string? s)]}
  "returns s as integer is possible, else nil"
  (try
    (Integer/parseInt s)
    (catch NumberFormatException e nil)))

; if function is needed
(defn parse-int [x]
  (Integer/parseInt x))

(defn merge-if [p? m1 m2]
  "if p?, returns m1 and m2 merged, else m1"
  (if p? (merge m1 m2) m1))

(defn keyword-from-tag-str [s]
  (when (.startsWith s "#")
    (keyword (.substring s 1))))

(defn tag-str-from-keyword [k]
  (when (keyword? k)
    (str "#" (name k))))

(defn substring-of? [s1 s2]
  "true if s1 is a substring of s2"
  (when-not (or (nil? s1) (nil? s2))
    (< -1 (.indexOf s2 s1))))

(defn supstring-of? [s1 s2]
  "true if s2 is a substring of s1"
  (when-not (or (nil? s1) (nil? s2))
    (< -1 (.indexOf s1 s2))))

(defn assoc-if [p? m k v]
  (if p? (assoc m k v) m))     

(defn assoc-with 
  "assoc with key k in map m (f v) of the current value v"
  ([m k f] (let [curr-v (get m k)]
             (assoc m k (f curr-v))))
  ([m k f & kfs]
     (let [tmp-m (assoc-with m k f)]
       (apply assoc-with tmp-m kfs))))

(defn assert-non-empty [x]
  (if (empty? x)
    (assert false)
    x))

(defn contained-in? [x coll]
  (not (nil? (some #{x} coll))))

(defn map-corresp?
  "tests whether two maps correspond in given keys,
   or all if none is given"
  ([m1 m2] (= m1 m2))
  ([m1 m2 k] (= (k m1) (k m2)))
  ([m1 m2 k & ks] (if (map-corresp? m1 m2 k)
                    (apply map-corresp? m1 m2 ks)
                    false)))

(defn non-corresp-keys
  "list keys of given maps that are not equal,
   ignoring given keys (excl)"
  ([m1 m2]
     (let [ks (seq (set (concat (keys m1) (keys m2))))]
       (loop [q ks, result []]
         (if-let [k (first q)]
           (if (map-corresp? m1 m2 k)
             (recur (rest q) result)
             (recur (rest q) (conj result k)))
           result))))
  ([m1 m2 excl-k]
     (let [all (non-corresp-keys m1 m2)]
       (filter #(not= % excl-k) all)))
  ([m1 m2 excl-k & excl-ks]
     (let [all  (non-corresp-keys m1 m2)
           excl (cons excl-k excl-ks)]
       (filter #(not (contained-in? % excl)) all))))
