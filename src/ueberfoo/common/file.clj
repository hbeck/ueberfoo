(ns ueberfoo.common.file
  (:import [java.io File]))

(defn empty-dir? [dir]
  (let [c (class dir)
        f (cond
           (= c java.io.File) dir
           (= c java.lang.String) (File. dir)
           :else nil)]
    (when (and (not (nil? f)) (.isDirectory f))
      (zero? (count (.list f))))))

(defn subdirs [dir]
  (filter #(.isDirectory %) (.listFiles dir)))

(defn hasSubdirs? [dir]
  (not (zero? (count (subdirs dir)))))
