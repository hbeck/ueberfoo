;; created 2012-01-14

(ns ueberfoo.commandline
  (:use [ueberfoo.common])
  (:use [ueberfoo.parsing]))

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

(defn mk-new-file-map []
  {:created (timestamp-str)
   :max-id  0
   :entries []})

(defn update-map [m entries]
  (assoc m
    :updated (timestamp-str)
    :max-id  (inc (:max-id m))
    :entries entries))

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

(defn file-new [filename]
  (do
    (spit filename (mk-new-file-map))
    (println (str "created new file " filename "."))))

(defn file-add-entry [filename words]
  (let [m     (load-from-file filename)
        entry (assoc (mk-new-entry words) :id (inc (:max-id m)))
        new-m (assoc m
                :updated (timestamp-str)
                :max-id  (inc (:max-id m))
                :entries (conj (:entries m) entry))]
    (do
      (prn entry)
      (spit filename new-m)
      (let [n (count (:entries new-m))]
        (if (= 1 n)
          (println "1 entry.")
          (println (str n " entries.")))))))

;; common part of list and delete
(defn mk-filter-struct [filename & [options]]
  ;; 'result' stems from vertical filtering
  ;; 'selected' stems from horizontal (key selection) filtering of 'result'
  (let [file-map    (load-from-file filename)
        entries     (:entries file-map)
        opt-map     (parse-list-options (mk-options-vec options))
        filtered    (filter (mkfn-filter opt-map) entries) ;; -f
        opt-map-n   (assoc opt-map :nr-of-entries (count filtered))
        result      ((mkfn-list-post-filter opt-map-n) filtered) ;; -l -r -o
        selected    (map (mkfn-selector opt-map) result ) ;; -s -*
        formatter   (mkfn-entry-formatter opt-map)] ;; -c -d
    {:file-map file-map
     :entries entries
     :opt-map opt-map
     :result result
     :selected selected
     :formatter formatter}))

(defn file-list-entries [filename & [options]]
  (let [in          (mk-filter-struct filename options)
        opt-map     (:opt-map in)        
        one-liners? (and (not (:all opt-map)) (= 1 (count (:select opt-map))))
        printer     (if one-liners? print println)]
    (do
      (doseq [x (:selected in)] (printer ((:formatter in) x)))
      (if (:verbose opt-map) ;; -v
        (println (str (count (:selected in)) "/" (count (:entries in)) " entries."))))))

(defn file-delete-entries [filename options]
  {:pre [(not (empty? options))]}
  (let [in           (mk-filter-struct filename options)
        to-delete    (:result in) ;; do not use :selected (entry equality!)
        entries2keep (vec (filter #(not (contained-in? % to-delete)) (:entries in)))
        n-del        (- (count (:entries in)) (count entries2keep))
        new-m        (assoc (:file-map in)
                       :updated (timestamp-str)
                       :entries entries2keep)]
    (do
      (spit filename new-m)
      (print n-del)
      (cond
       (zero? n-del) (println " entries deleted.")
       (= 1 n-del)   (println " entry deleted:")
       :else         (println " entries deleted:"))
      (doseq [x (:selected in)] (println ((:formatter in) x))))))

;;; invocation

(defn usage []
  (println
   (str "usage:\n\n"
                
        "FILENAME new\n"
        "FILENAME add TEXT\n"
        "FILENAME list [OPTIONS]\n"
        "FILENAME delete OPTIONS\n\n"
                
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
  (let [[filename cmd & args] *command-line-args*]
    (case cmd
          "help"   (usage)
          "new"    (file-new filename)
          "add"    (file-add-entry filename args)
          "list"   (file-list-entries filename args)
          "delete" (file-delete-entries filename args)
          (usage))))

(run-script)


