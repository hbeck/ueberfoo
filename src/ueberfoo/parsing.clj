;; created 2012-01-16

(ns ueberfoo.parsing
  (:use [ueberfoo.common]))

;;; parse entry helpers

(defn mk-key-from-vc [str-vc]
  "[\"a\" \"b\"] --> :ab"
  (if (empty? str-vc)
    (do
      (prn "empty string after #")
      (assert false))
    (keyword (apply str str-vc))))

(defn mk-entry [m]
  (let [tags (:tags m)]    
    (merge {:created (timestamp-str)
            :text    (.trim (apply str (:text-vc m)))}
           (when-not (empty? tags) {:tags tags})
           (apply hash-map (:kv-vc m)))))

(defn add-curr-tag [m]
  (merge-with conj m {:tags (mk-key-from-vc (:curr-k-vc m))}))

(defn init-prs-val [m]
  "adds current key to kv-vc and prepares empty vc for value parsing"
    (merge-with conj
                (assoc m :curr-v-vc [])
                {:kv-vc (mk-key-from-vc (:curr-k-vc m))}))

(defn add-curr-val [m]
  "add current value to kv-vc"
  (let [v (apply str (assert-non-empty (:curr-v-vc m)))]
    (merge-with conj m {:kv-vc v})))

(defn conj-to [m k v]
  (merge-with conj m {k v}))

;;; parse entry

(defn parse-entry [s]
  "parses string s and new entry map (sans :id)"
  (letfn
      ;; x is the current char, map m holds the context
      ;; recognizing "" means end of parsing
      [(prs-txt [m [x & xs]]
         ;;withing regular text: built in :text-vc
         #(case (str x)
                ""  (mk-entry m)
                "#" (prs-key (assoc m :curr-k-vc []) xs)
                    (prs-txt (conj-to m :text-vc x) xs)))
       ;
       (prs-key [m [x & xs]]
         ;;within tag or key part of key-value pair: built in curr-k-vc
         #(case (str x)
                ""  (mk-entry (add-curr-tag m))
                "=" (prs-val  (init-prs-val m) xs)
                " " (prs-txt  (add-curr-tag m) xs)
                    (prs-key  (conj-to m :curr-k-vc x) xs)))
       ;
       (prs-val [m [x & xs]]
         ;; within value part of key-value pair: built in curr-v-vc
         #(case (str x)                
                ""  (mk-entry (add-curr-val m))
                " " (prs-txt  (add-curr-val m) xs)
                    (prs-val  (conj-to m :curr-v-vc x) xs)))]         
    ;; start parsing
    (trampoline
      prs-txt
      {:text-vc [], :tags #{}, :kv-vc []} ;; m
      (vec s)))) ;; [x & xs]


;;; parse list options helpers

; optimistic tests
(defn- kv? [x] (substring-of? "=" x))
(defn- tag? [x] (.startsWith x "#"))

(defn mk-kv [s]
  (let [toks (.split s "=")
        k    (keyword-from-tag-str (get toks 0))
        v    (if (= (count toks) 1)
               :* 
               (get toks 1))]
    {k v}))

(defn mk-token [s]
  (cond
   (kv? s)  (mk-kv s)                ;; "#k=v" --> {:k "v"}
   (tag? s) (keyword-from-tag-str s) ;; "#t"   --> :t
   :else    s))                      ;; "s"    --> "s"

(def default-options
  {:select [:text], :display "v"})

;;; parse list options

; note: since forward pointers are not possible, mutually recursive
; functions (for trampoline) must be defined within the same letfn.

(defn parse-list-options [options-vc]
  {:pre [(true? (or (empty? options-vc) (reduce l-and (map string? options-vc))))]}
  "options-vc: vector of arguments (strings).
   returns map representation."
  (when-not (nil? options-vc)
    (letfn
        [(read-next [m [x & xs]]
           "read next option and determine respective next state"
           #(cond
             ;; entry filter
             (or (= "-f" x) (= "--filter" x))  (conj-to-with m :filter mk-token [] xs)
             ;; filtered list post processing
             (or (= "-r" x) (= "--reverse" x)) (read-next (assoc m :reverse true) xs)
             (or (= "-l" x) (= "--limit" x))   (assoc-next-with m :limit parse-int xs)
             (or (= "-o" x) (= "--offset" x))  (assoc-next-with m :offset parse-int xs)
             ;; key selection
             (or (= "-s" x) (= "--select" x))  (conj-to-with m :select keyword [] xs)
             (or (= "-*" x) (= "--all" x))     (read-next (assoc m :all true) xs)
             ;; output format
             (or (= "-c" x) (= "--clj" x))     (read-next (assoc m :clj true) xs)
             (or (= "-d" x) (= "--display" x)) (assoc-next-with m :display identity xs)
             ;; additional output control
             (or (= "-v" x) (= "--verbose" x)) (read-next (assoc m :verbose true) xs)
             ;;
             (empty? x) m
             :else (do (println "unknown option: " x) (assert false))))
         ;
         (conj-to-with [m k f vc [x & xs]]
           "in the value for k, conj (f x)"
           #(cond
             (empty? x)          (assoc m k (assert-non-empty vc))
             (.startsWith x "-") (read-next (assoc m k (assert-non-empty vc)) (vec (cons x xs)))
             :else               (conj-to-with m k f (conj vc (f x)) xs)))
         ;
         (assoc-next-with [m k f [x & xs]]
           "associate key k in map m with (f x)"
           #(read-next (assoc m k (f (assert-non-empty x))) xs))]
      ;; start parsing
      (trampoline read-next default-options options-vc))))