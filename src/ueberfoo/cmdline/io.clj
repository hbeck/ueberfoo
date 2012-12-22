(ns ueberfoo.cmdline.io
  (:use [ueberfoo.cmdline.ds])
  (:use [ueberfoo.common.date])
  (:use [ueberfoo.common.filter])
  (:use [ueberfoo.common.conversion])
  (:import [java.util Date])
  (:import [java.io File]))

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
                               (and (.startsWith name "id")
                                    (.endsWith name ".clj")))
                            all-files)]
    (map load-from-file entry-files)))

(defn db-get-entry-by-id [dirname args]
  "args: first string has to be id"
  (let [id      (parse-int (first args))
        entries (load-all-entries dirname)]
    (find-first #(= (:id %)) entries)))

