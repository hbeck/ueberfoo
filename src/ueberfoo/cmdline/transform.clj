(ns ueberfoo.cmdline.transform)

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
  "decides whether key, value, or both are displayed"
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
