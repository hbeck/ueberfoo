;; created 2012-01-16 00:43
;; author hbeck

(ns ueberfoo.parsing
  (:use [ueberfoo.static])
  (:use [ueberfoo.genfuns]))

(defn parse-list-options [options-vec]
  "collects options in a map"
  (when-not (nil? options-vec)
    (letfn
        [(parse-int [x] (Integer/parseInt x))
;;         (parse-date [x] (.parse (java.text.SimpleDateFormat. "yyyy-MM-dd HH:mm:ss") x))
         (with-not-empty-for [e x k]
           (if (empty? x)
             (do
               (println (str "expected value for " k))
               (assert false))
             e))
         (str-or-tag-or-kv [s]
           (cond
            (substring-of? "=" s)
            (let [tks (.split s "=")
                  k   (tag-str-to-keyword (get tks 0))]
              (if (= (count tks) 1) ;; then key existence suffices
                {k :any}
                {k (get tks 1)})),
            (.startsWith s "#") (tag-str-to-keyword s)
            :else s))
         (read-next [m [x & xs]]                     
           #(cond
             ;; selection
             (or (= "-s" x) (= "--select" x))  (conj-with m :select keyword [] xs)
             (or (= "-*" x) (= "--all" x))     (read-next (assoc m :all true) xs)
             ;; filter
             (or (= "-f" x) (= "--filter" x))  (conj-with m :filter str-or-tag-or-kv [] xs)
;;             (or (= "-a" x) (= "--after" x))   (assoc-next-with m :after parse-date xs)
;;             (or (= "-b" x) (= "--before" x))  (assoc-next-with m :before parse-date xs)
             ;; output control
             (or (= "-r" x) (= "--reverse" x)) (read-next (assoc m :reverse true) xs)
             (or (= "-l" x) (= "--limit" x))   (assoc-next-with m :limit parse-int xs)
             (or (= "-o" x) (= "--offset" x))  (assoc-next-with m :offset parse-int xs)
             ;; output format
             (or (= "-v" x) (= "--verbose" x)) (read-next (assoc m :verbose true) xs)
             (or (= "-c" x) (= "--clj" x))     (read-next (assoc m :clj true) xs)
             (or (= "-d" x) (= "--display" x)) (assoc-next-with m :display identity xs)
             (empty? x) m
             :else (do (println "unknown option: " x) (assert false))))
         (conj-with [m k f tmp [x & xs]]
           "associate with key a set of items (f x)"
           #(cond
             (nil? x) (with-not-empty-for (assoc m k tmp) tmp k),
             (.startsWith x "-")
             (with-not-empty-for (read-next (assoc m k tmp) (vec (cons x xs))) tmp k),
             :else (conj-with m k f (conj tmp (f x)) xs)))
         (assoc-next-with [m k f [x & xs]]
           "associate key k in map m with (f x)"
           #(with-not-empty-for (read-next (assoc m k (f x)) xs) x k))]
      (trampoline
       read-next
       {:select [:text], :display "v"} ;; default
       options-vec))))

(defn conj-tag [tags v]
  "appends keyword for given vec if it is not empty"
  (if (empty? v)
    tags
    (conj tags (keyword (apply str v)))))

(defn parse-text [s m]
  {:pre [(not (nil? (:max-id m)))]}
  "parses string s as new entry. map meta has to provide meta info,
   currently only max-id needed."
  (let [xs (vec s),
        make-entry 
        (fn [text-vec tags kvs]
          (let [base {:id (inc (:max-id m))
                      :created (make-timestamp-str)
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

