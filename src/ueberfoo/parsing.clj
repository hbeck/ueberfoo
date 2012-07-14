;; created 2012-01-16 00:43
;; author Harald Beck

(ns ueberfoo.parsing
  (:use [ueberfoo.common]))

(defn parse-list-options [options-vec]
  "returns an option map"
  (when-not (nil? options-vec)
    (letfn
        [(parse-int [x] (Integer/parseInt x))
         (assert-non-empty-v [k v]
           (if (empty? v)
             (do
               (println (str "expected value for " k))
               (assert false))
             v))             
         (str-or-tag-or-kv [s]
           (letfn [(kv?  [x] (substring-of? "=" x))
                   (tag? [x] (.startsWith s "#"))]
             (cond
              (kv? s) (let [toks (.split s "=")
                            k    (tag-str-to-keyword (get toks 0))]
                        (if (= (count toks) 1) ;; then key existence suffices
                          {k :any}
                          {k (get toks 1)}))
              (tag? s) (tag-str-to-keyword s)
              :str s)))
         (read-next [m [x & xs]]                     
           #(cond
             ;; key selection
             (or (= "-s" x) (= "--select" x))  (conj-with m :select keyword [] xs)
             (or (= "-*" x) (= "--all" x))     (read-next (assoc m :all true) xs)
             ;; entry filter
             (or (= "-f" x) (= "--filter" x))  (conj-with m :filter str-or-tag-or-kv [] xs)
             ;; result list post processing
             (or (= "-r" x) (= "--reverse" x)) (read-next (assoc m :reverse true) xs)
             (or (= "-l" x) (= "--limit" x))   (assoc-next-with m :limit parse-int xs)
             (or (= "-o" x) (= "--offset" x))  (assoc-next-with m :offset parse-int xs)
             ;; output format
             (or (= "-c" x) (= "--clj" x))     (read-next (assoc m :clj true) xs)
             (or (= "-d" x) (= "--display" x)) (assoc-next-with m :display identity xs)
             ;; additional output control
             (or (= "-v" x) (= "--verbose" x)) (read-next (assoc m :verbose true) xs)
             ;;
             (empty? x) m
             :else (do (println "unknown option: " x) (assert false))))
         (conj-with [m k f tmp [x & xs]]
           "associate with key k a set of items (f x)"
           #(cond
             (nil? x)            (assoc m k (assert-non-empty-v k tmp))
             (.startsWith x "-") (read-next (assoc m k (assert-non-empty-v k tmp)) (vec (cons x xs)))
             :else               (conj-with m k f (conj tmp (f x)) xs)))
         (assoc-next-with [m k f [x & xs]]
           "associate key k in map m with (f x)"
           #(read-next (assoc m k (f (assert-non-empty-v k x))) xs))]
      (trampoline
       read-next
       {:select [:text], :display "v"} ;; default
       options-vec))))

(defn conj-tag [tags v]
  "appends keyword for given vec if it is not empty"
  (if (empty? v)
    tags
    (conj tags (keyword (apply str v)))))

(defn parse-text [s]
  "parses string s as new entry sans :id"
  (let [xs (vec s),
        make-entry 
        (fn [text-vec tags kvs]
          (let [base {:created (make-timestamp-str)
                      :text (.trim (apply str text-vec))}
                with-tags (assoc-if (not (nil? tags)) base :tags tags)
                with-kvs  (merge with-tags (apply hash-map kvs))]
            with-kvs))]
    (letfn
        [(normal-text [text-vec tags kvs [x & xs]]
           #(case x
                  nil (make-entry  text-vec          tags kvs)
                  \#  (within-key  text-vec          tags kvs xs, [])
                      (normal-text (conj text-vec x) tags kvs xs)))
         (within-key [text-vec tags kvs [x & xs], key-vec]
           #(case x
                  nil (make-entry   text-vec (conj-tag tags key-vec) kvs)
                  \=  (within-value text-vec tags                    kvs xs,
                                    (keyword (apply str key-vec)) [])
                  \   (normal-text  text-vec (conj-tag tags key-vec) kvs xs)
                      (within-key   text-vec tags                    kvs xs,
                                    (conj key-vec x))))
         (within-value [text-vec tags kvs [x & xs], key value-vec]
           #(case x
                  nil (assoc-if (not (empty? value-vec)),
                                (make-entry text-vec tags kvs),
                                key (apply str value-vec))                                
                  \   (normal-text  text-vec tags (conj kvs key (apply str value-vec)) xs)
                      (within-value text-vec tags kvs xs, key (conj value-vec x))))]
      (trampoline normal-text [] #{} [] xs))))

