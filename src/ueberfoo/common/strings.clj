(ns ueberfoo.common.strings)

(defn substring-of? [s1 s2]
  "true if s1 is a substring of s2"
  (when-not (or (nil? s1) (nil? s2))
    (< -1 (.indexOf s2 s1))))

(defn supstring-of? [s1 s2]
  "true if s2 is a substring of s1"
  (when-not (or (nil? s1) (nil? s2))
    (< -1 (.indexOf s1 s2))))
