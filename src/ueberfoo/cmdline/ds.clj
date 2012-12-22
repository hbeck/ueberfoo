(ns ueberfoo.cmdline.ds
  (:use [ueberfoo.common common date])
  (:use [ueberfoo.cmdline.parsing]))

;;; non-IO parts of file access functions

(defn mk-new-db-map []
  {:created (timestamp-str)
   :format  {:name "ueberfoo",
             :version "0.0.3"}
   :max-id  0})

(defn mk-new-entry [words]
  "words: vector of strings. returns entry sans :id."
  (let [text (apply concat (map #(concat % " ") words))]
    (parse-entry text)))

(defn mk-options-vec [options]
  "returns a vector containing command line options"
  (cond
   (empty? options) [],
   (coll? options) (vec options),
   (string? (first options)) (vec (.split (first options) " ")),
   :else (do (println "unknown class for options arg" (class options))
             (assert false))))
