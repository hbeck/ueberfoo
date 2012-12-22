;; created 2012-01-16

(ns ueberfoo.common.common)

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

(defn assert-non-empty [x]
  (if (empty? x)
    (assert false)
    x))

(defn contained-in? [x coll]
  (not (nil? (some #{x} coll))))

