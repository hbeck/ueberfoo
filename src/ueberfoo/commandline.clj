;; created 2012-01-14
;; author Harald Beck

(ns ueberfoo.commandline
  (:use [ueberfoo.common])
  (:use [ueberfoo.parsing]))

;;; list transformation

(defn mk-list-post-processor [opt-map]
  {:pre [(contains? opt-map :nr-of-entries)]}
  (let [order-fn (if (:reverse opt-map) reverse identity)
        offset   (if-let [n (:offset opt-map)] n 0)
        limit    (if-let [n (:limit opt-map)] n (:nr-of-entries opt-map))]
    (comp vec #(take limit (drop offset %)) order-fn)))

(defn format-value [v]
  (if (not (set? v))
    v
    (let [xs (for [x v]
               (if (keyword? x)
                 (str "#" (name x) " ")
                 (str x " ")))]
      (.trim (apply str xs)))))

(defn- mk-kv-formatter [d]
  (cond
   (or
    (nil? d)
    (= "kv" d)) #(str (name (get % 0)) ": " (format-value (get % 1)))
    (= "k" d)   #(str (name (get % 0)))
    (= "v" d)   #(format-value (get % 1))))

(defn- line [kv-formatter kv-pair]
    (str (apply str (kv-formatter kv-pair) "\n")))

(defn mk-entry-formatter [opt-map]
  (if (:clj opt-map)
    identity
    (let [kv-formatter (mk-kv-formatter (:display opt-map))]
      (fn [entry]
        (apply str (for [kv-pair entry] (line kv-formatter kv-pair)))))))

(defn mk-selector [opt-map]
  (if (:all opt-map)
    identity
    #(select-keys % (reverse (:select opt-map)))))


;; helper fns for mk-filter

(defn- mk-all-text-tokens? [f-txts]
  (fn [entry]
    (or (empty? f-txts)
        (let [text (:text entry)]
          (apply l-and (map (partial supstring-of? text) f-txts))))))

(defn- mk-all-tags? [f-tags]
  (fn [entry]
    (or (empty? f-tags)
        (let [tags (:tags entry)]
          (apply l-and (map (partial contains? tags) f-tags))))))

(defn- mk-key-matches-filtered? [k op filtered]
  (fn [entry]
    (or (empty? filtered)
        (let [xs (k entry)]
          (apply op (map (partial contains? xs) filtered))))))

;; TODO

(defn- mk-all-kvs? [f-kvs]
  (fn [entry]
    (or (empty? f-kvs)
        (apply
         l-and
         (for [e f-kvs]
           (let [kv      (first e)
                 test-k  (get kv 0)
                 test-v  (get kv 1)
                 entry-v (test-k entry)]
             (cond
              ;; special case for "key=" (w/o value)
              (= :any test-v) (not (nil? entry-v)), ;; then that suffices
              (can-as-number? test-v) (and (can-as-number? entry-v)
                                           (= (as-number test-v) (as-number entry-v))),
              :else (substring-of? test-v entry-v))))))))
  
;; filter

(defn mk-filter [opt-map]
  (let [xs (:filter opt-map)]
    (if (empty? xs)
      identity
      (fn [entry] (and ((mk-all-text-tokens? (filter string? xs)) entry)
                       ((mk-all-tags? (filter keyword? xs)) entry)
                       ((mk-all-kvs? (filter map? xs)) entry))))))

      ;; (fn [entry]
      ;;   (reduce
      ;;    l-and
      ;;    (for [pred [(mk-key-matches-filtered? :text l-and (filter string? xs))
      ;;                (mk-key-matches-filtered? :tags l-and (filter keyword? xs))
      ;;                (mk-all-kvs? (filter map? xs))]]
      ;;      (pred entry)))))))
          

;;; non-io parts of file access functions

(defn update-map [m entries]
  (assoc m
    :format ueberfoo-format
    :updated (make-timestamp-str)
    :max-id (inc (:max-id m))
    :entries entries))

(defn make-new-file-map []
  {:format  ueberfoo-format
   :created (make-timestamp-str)
   :max-id  0
   :entries []})

(defn make-new-entry [words]
  (let [text (apply concat (map #(concat % " ") words))]
    (parse-text text)))

(defn make-options-vec [options]
  (cond
   (nil? options) []
   (coll? options) (vec options)
   (= java.lang.String (class (first options))) (vec (.split (first options) " "))
   :else (do (println "unknown class for options arg" (class options))
             (assert false))))


;;; file access

(defn file-load-ueberfoo [filename]
  (load-string (slurp filename)))

(defn file-new [filename]
  (do
    (spit filename (make-new-file-map))
    (println (str "created new file " filename "."))))

(defn file-add-entry [filename words]
  (let [m     (file-load-ueberfoo filename)
        x     (assoc (make-new-entry words) :id (inc (:max-id m)))
        new-m (update-map m (conj (:entries m) x))]
    (do
      (prn x)
      (spit filename new-m)
      (let [n (count (:entries new-m))]
        (if (= 1 n)
          (println "1 entry.")
          (println (str n " entries.")))))))

(defn file-list-entries [filename & [options]]
  (let [xs          (:entries (file-load-ueberfoo filename))
        options-vec (make-options-vec options)
        opt-map     (parse-list-options options-vec)
        filtered    (filter (mk-filter opt-map) xs)
        selected    (map (mk-selector opt-map) filtered)
        opt-map-n   (assoc opt-map :nr-of-entries (count selected))
        result      ((mk-list-post-processor opt-map-n) selected)
        xform       (mk-entry-formatter opt-map)
        one-liners? (and (not (:all opt-map)) (= 1 (count (:select opt-map))))
        printer     (if one-liners? print println)]
    (do
      (doseq [x result] (printer (xform x)))
      (if (:verbose opt-map)
        (println (str (count result) "/" (count xs) " entries."))))))

(defn file-delete-entries [filename & [options]]
  {:pre [(not (empty? options))]}
  (let [m           (file-load-ueberfoo filename)
        xs          (:entries m)
        options-vec (make-options-vec options)
        opt-map     (parse-list-options options-vec)
        filtered    (filter (mk-filter opt-map) xs)
        ids         (set (map :id filtered))
        other-xs    (vec (filter #(not (contains? ids (:id %))) xs))
        new-m       (update-map m other-xs)]
    (do
      (spit filename new-m)
      (let [n (count ids)]
        (if (= 1 n)
          (println "1 entry deleted.")
          (println (str n " entries deleted.")))))))


;;; invocation

(defn usage []
  (println (str "usage:\n"
                "FILENAME new\n"
                "FILENAME add TEXT\n"
                "FILENAME list [options]")))

(defn- run-script []
  (let [[filename cmd & r] *command-line-args*]
    (case cmd
          "help"   (usage)
          "new"    (file-new filename)
          "add"    (file-add-entry filename r)
          "list"   (file-list-entries filename r)
          "delete" (file-delete-entries filename r)
          (usage))))

(run-script)


