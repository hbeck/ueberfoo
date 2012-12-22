;; created 2012-01-14

(ns ueberfoo.cmdline.main
  (:use [ueberfoo.common common conversion date file filter maps strings queries])
  (:use [ueberfoo.cmdline parsing ds io transform])
  (:use [clojure.set :only [difference]])
  (:import [java.util Date])
  (:import [java.io File]))

;; common part of list and delete
(defn mk-filter-ctxt [dirname & [options]]
  ;; 'result' stems from vertical filtering
  ;; 'selected' stems from horizontal (key selection) filtering of 'result'
  (let [db          (load-from-file (db-path dirname))
        entries     (load-all-entries dirname)
        opt-map     (parse-list-options (mk-options-vec options))
        filtered    (filter (mkfn-filter opt-map) entries) ;; -f
        opt-map-n   (assoc opt-map :nr-of-entries (count filtered))
        sorted      (sort (fn [x y] (<= (:id x) (:id y))) filtered)
        result      ((mkfn-list-post-filter opt-map-n) sorted) ;; -l -r -o
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

(defn print-tag-rank [entries ctxt]
  (let [sorted    (tag-rank entries)
        opt-map-n (assoc (:opt-map ctxt) :nr-of-entries (count entries))
        result    ((mkfn-list-post-filter opt-map-n) sorted)] ;; -l -r -o
    (doseq [x result] (println (get x 0) (get x 1)))))

(defn query [dirname & [options]]  
  (let [ctxt        (mk-filter-ctxt dirname (rest options))
        opt-vec     (mk-options-vec options)
        entries     (:entries ctxt)
        queryname   (first opt-vec)
        args        (rest opt-vec)]
    (cond
     (= queryname "tag-rank") (print-tag-rank entries ctxt)
     :else (println "unknown queryname: " queryname))))

;;; invocation

(defn usage []
  (println
   (str "usage:\n\n"
                
        "DIR new\n"
        "DIR add TEXT\n"
        "DIR list [OPTIONS]\n"
        "DIR delete OPTIONS\n"
        "DIR query QUERYNAME [ARGS]\n\n"
                
        "OPTIONS\n"
        "filtering:\n"
        "-f  --filter MATCH:\n"
        "string, #tag, #key=, #key=substr\n"
        ;;"    --from DATE\n"
        ;;"    --to   DATE\n"
        ;;"DATE := YYYY-MM-DD | today | yesterday\n"
        ;;"-t  abbreviates --from today\n\n"
                
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
          "query"  (query dirname args)
          (usage))))

(run-script)


