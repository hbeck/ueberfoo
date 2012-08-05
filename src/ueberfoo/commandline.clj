;; created 2012-01-14

(ns ueberfoo.commandline
  (:use [ueberfoo.common])
  (:use [ueberfoo.parsing])
  (:use [clojure.set :only [difference]])
  (:import [java.util Date])
  (:import [java.io File]))


;;; list transformation

(defn mkfn-list-post-filter [opt-map]
  {:pre [(contains? opt-map :nr-of-entries)]}
  (let [order-fn (if (:reverse opt-map) reverse identity)
        offset   (if-let [n (:offset opt-map)] n 0)
        limit    (if-let [n (:limit opt-map)] n (:nr-of-entries opt-map))]
    (comp vec #(take limit (drop offset %)) order-fn)))

(defn string-repr [x]
  (cond
   (string? x)  x
   (number? x)  (str x)
   (keyword? x) (str "#" (name x))
   (coll? x)    (.trim (apply str (map #(str (string-repr %) " ") x)))
   :else        ""))  

(defn mkfn-kv-formatter [d]
  "decides wether key, value, or both are displayed"
  (cond
    (= "kv" d) #(str (string-repr (get % 0)) ": " (string-repr (get % 1)))
    (= "k"  d) #(str (string-repr (get % 0)))
    (= "v"  d) #(string-repr (get % 1))))

(defn line [kv-formatter kv-pair]
    (str (apply str (kv-formatter kv-pair) "\n")))

(defn mkfn-entry-formatter [opt-map]
  (if (:clj opt-map)
    identity
    (let [kv-formatter (mkfn-kv-formatter (:display opt-map))]
      (fn [entry]
        (apply str (for [kv-pair entry] (line kv-formatter kv-pair)))))))

(defn mkfn-selector [opt-map]
  (if (:all opt-map)
    identity
    #(select-keys % (reverse (:select opt-map)))))
  
;; filter

(defn matches? [entry key-f pred? junct tokens]
  (case junct
        :all (or (empty? tokens)
                 (apply l-and (map (partial pred? (key-f entry)) tokens)))
        :any (apply l-or (map (partial pred? (key-f entry)) tokens))
        (do (println "unknown junct:" junct) (assert false))))

(defn matches-all-text? [entry txt-tokens]
  (matches? entry :text supstring-of? :all txt-tokens))

(defn matches-any-text? [entry txt-tokens]
  (matches? entry :text supstring-of? :any txt-tokens))

(defn matches-all-tags? [entry tag-tokens]
  (matches? entry :tags contains? :all tag-tokens))

(defn matches-any-tags? [entry tag-tokens]
  (matches? entry :tags contains? :any tag-tokens))

(defn matches-kv? [entry k test-v]
  (let [v (get entry k)]
    (if (nil? v)
      false
      (cond
       (= :* test-v) true
       (keyword? test-v) (= v test-v)
       (string? test-v)  (substring-of? test-v v)
       :else (do (println ("value of key " k " has unkown class: " (class v)))
                 (assert false))))))  

(defn matches-all-kvs? [entry kv-tokens]
  (loop [[kv & kvs] kv-tokens]
    (if (empty? kv)
      true
      (let [vc (apply vec kv)] ;; [k v]
        (if (matches-kv? entry (get vc 0) (get vc 1))
          (recur kvs)
          false)))))

(defn mkfn-filter [opt-map]
  (let [flt-tokens (:filter opt-map)]
    (if (empty? flt-tokens)
      identity
      (fn [entry]
        (and (matches-all-text? entry (filter string? flt-tokens))
             (matches-all-tags? entry (filter keyword? flt-tokens))
             (matches-all-kvs? entry (filter map? flt-tokens)))))))

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

;;; IO. file access

(defn load-from-file [filename]
  (load-string (slurp filename)))

(defn db-path [dirname]
  "db.clj holds the meta information.
   return full path str to db.clj"  
  (str dirname "/db.clj"))

(defn db-new [dirname]
  (do
    (.mkdir (File. dirname))
    (spit (db-path dirname) (mk-new-db-map))
    (println (str "created new db " dirname "."))))

(defn mk-entry-subdirs [dirname date]
  "creates year/month subdirs if necessary and returns full path,
   e.g., /home/john/dbname/2012/8"
  (let [y (year-of date)
        m (month-of date)        
        s (str dirname "/" y "/" m)]
  (do
    (.mkdirs (File. s))
    s)))

(defn mk-entry-filename [id]
  (str "id" id ".clj"))

(defn mk-entry-path [dirname year month id]
  "[dirname]/[year]/[month]/id[id].clj"
  (str dirname "/" year "/" month "/" (mk-entry-filename id)))  

(defn db-add-entry [dirname words]
  (let [db     (load-from-file (db-path dirname))
        id     (inc (:max-id db))
        entry  (assoc (mk-new-entry words) :id id)
        new-db (assoc db
                 :updated (timestamp-str)
                 :max-id  id)
        subdir (mk-entry-subdirs dirname (Date.))]
    (do
      (prn entry)      
      (spit (db-path dirname) new-db)
      (spit (str subdir "/" (mk-entry-filename id)) entry))))

(defn load-all-entries [dirname]  
  (let [dir         (clojure.java.io/file dirname)
        all-files   (file-seq dir)
        entry-files (filter #(let [name (.getName %)]
                               (and (.startsWith name "id") (.endsWith name ".clj")))
                            all-files)]
    (map load-from-file entry-files)))


;; common part of list and delete
(defn mk-filter-ctxt [dirname & [options]]
  ;; 'result' stems from vertical filtering
  ;; 'selected' stems from horizontal (key selection) filtering of 'result'
  (let [db          (load-from-file (db-path dirname))
        entries     (load-all-entries dirname)
        opt-map     (parse-list-options (mk-options-vec options))
        filtered    (filter (mkfn-filter opt-map) entries) ;; -f
        opt-map-n   (assoc opt-map :nr-of-entries (count filtered))
        result      ((mkfn-list-post-filter opt-map-n) filtered) ;; -l -r -o
        selected    (map (mkfn-selector opt-map) result ) ;; -s -*
        formatter   (mkfn-entry-formatter opt-map)] ;; -c -d
    {:db db
     :entries entries
     :opt-map opt-map
     :result result
     :selected selected
     :formatter formatter}))

(defn db-list-entries [dirname & [options]]
  (let [ctxt        (mk-filter-ctxt dirname options)
        opt-map     (:opt-map ctxt)        
        one-liners? (and (not (:all opt-map)) (= 1 (count (:select opt-map))))
        printer     (if one-liners? print println)]
    (do
      (doseq [x (:selected ctxt)] (printer ((:formatter ctxt) x)))
      (if (:verbose opt-map) ;; -v
        (println (str (count (:selected ctxt)) "/" (count (:entries ctxt)) " entries."))))))

(defn next-unlisted-subdir [dir listed]
  (first (difference (set (subdirs dir)) (set listed))))

(defn delete-empty-subdirs [dirname]
  "search to leaf dir (= sans subdirs), and delete if empty; recur.
   returns deleted files"
  (loop [dir (File. dirname), to-decide [], kept [], deleted []]
    (if (nil? dir)
      deleted
      (if (hasSubdirs? dir)
        (let [subdir (next-unlisted-subdir dir (concat to-decide kept))]
          (if (nil? subdir) ;; all subdirs visited, ie cannot delete this one
            (recur (first to-decide) (rest to-decide) (conj kept dir) deleted)
            (recur subdir (cons dir to-decide) kept deleted)))
        (if (empty-dir? dir)
          (do
            (.delete dir)
            (recur (first to-decide) (rest to-decide) kept (conj deleted dir)))
          (recur (first to-decide) (rest to-decide) (conj kept dir) deleted))))))

(defn delete-entry [dirname entry]
  (let [date       (.parse sdf-date-time (:created entry))
        year       (year-of date)
        month      (month-of date)
        entry-path (mk-entry-path dirname year month (:id entry))]
    (do
      (.delete (File. entry-path)))))

(defn db-delete-entries [dirname options]
  {:pre [(not (empty? options))]}
  (let [ctxt           (mk-filter-ctxt dirname options)
        entries-to-del (:result ctxt) ;; do not use :selected (entry equality!)
        n              (count entries-to-del)
        new-db         (assoc (:db ctxt)
                         :updated (timestamp-str))]
    (do
      (spit (db-path dirname) new-db)
      (dorun (map #(delete-entry dirname %) entries-to-del))
      (delete-empty-subdirs dirname)
      (print n)
      (cond
       (zero? n) (println " entries deleted.")
       (= 1 n)   (println " entry deleted:")
       :else     (println " entries deleted:"))
      (doseq [x (:selected ctxt)] (println ((:formatter ctxt) x))))))

;;; invocation

(defn usage []
  (println
   (str "usage:\n\n"
                
        "DIR new\n"
        "DIR add TEXT\n"
        "DIR list [OPTIONS]\n"
        "DIR delete OPTIONS\n\n"
                
        "OPTIONS\n"
        "filtering:\n"
        "-f  --filter MATCH:\n"
        "string, #tag, #key=, #key=substr\n\n"
                
        "key selection\n"
        "-s  --select KEYS\n"
        "-*  --all\n\n"
                
         "filtered list post processing\n"
         "-r  --reverse\n"
         "-l  --limit NUMBER\n"
         "-o  --offset NUMBER\n\n"
                
         "output format\n"
         "-c  --clj\n"
         "-d  --display \"kv\"|\"k\"|\"v\"\n"
         "-v  --verbose")))

(defn run-script []
  (let [[dirname cmd & args] *command-line-args*]
    (case cmd
          "help"   (usage)
          "new"    (db-new dirname)
          "add"    (db-add-entry dirname args)
          "list"   (db-list-entries dirname args)
          "delete" (db-delete-entries dirname args)
          (usage))))

(run-script)


