(ns ueberfoo.common.maps
  (:use [ueberfoo.common common]))

(defn merge-if [p? m1 m2]
  "if p?, returns m1 and m2 merged, else m1"
  (if p? (merge m1 m2) m1))

(defn assoc-if [p? m k v]
  (if p? (assoc m k v) m))     

(defn assoc-with 
  "assoc with key k in map m (f v) of the current value v"
  ([m k f] (let [curr-v (get m k)]
             (assoc m k (f curr-v))))
  ([m k f & kfs]
     (let [tmp-m (assoc-with m k f)]
       (apply assoc-with tmp-m kfs))))

(defn map-corresp?
  "tests whether two maps correspond in given keys,
   or all if none is given"
  ([m1 m2] (= m1 m2))
  ([m1 m2 k] (= (k m1) (k m2)))
  ([m1 m2 k & ks] (if (map-corresp? m1 m2 k)
                    (apply map-corresp? m1 m2 ks)
                    false)))

(defn non-corresp-keys
  "list keys of given maps that are not equal,
   ignoring given keys (excl)"
  ([m1 m2]
     (let [ks (seq (set (concat (keys m1) (keys m2))))]
       (loop [q ks, result []]
         (if-let [k (first q)]
           (if (map-corresp? m1 m2 k)
             (recur (rest q) result)
             (recur (rest q) (conj result k)))
           result))))
  ([m1 m2 excl-k]
     (let [all (non-corresp-keys m1 m2)]
       (filter #(not= % excl-k) all)))
  ([m1 m2 excl-k & excl-ks]
     (let [all  (non-corresp-keys m1 m2)
           excl (cons excl-k excl-ks)]
       (filter #(not (contained-in? % excl)) all))))

