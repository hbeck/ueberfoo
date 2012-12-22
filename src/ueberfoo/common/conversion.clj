(ns ueberfoo.common.conversion)

(defn keyword-from-tag-str [s]
  (when (.startsWith s "#")
    (keyword (.substring s 1))))

(defn tag-str-from-keyword [k]
  (when (keyword? k)
    (str "#" (name k))))

(defn can-char-as-integer? [c]
  {:pre [(char? c)]}
  (try
    (do (Integer/parseInt (str c)) true)
    (catch NumberFormatException e false)))    

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

