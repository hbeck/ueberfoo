;; created 120114 0010
;; author hbeck

(ns ueberfoo.cmdline
  (:use [ueberfoo.genfuns])
  (:use [ueberfoo.parsing])
  (:use [ueberfoo.static]))

(defn new-file [filename]
  (let [m {:format  ueberfoo-format
           :created (make-timestamp-str)
           :max-id  0
           :entries []}]
    (do
      (spit filename m)
      (println (str "created new file " filename ".")))))

(defn load-ueberfoo [filename]
  (load-string (slurp filename)))

(defn load-entries [filename]
  (:entries (load-string (slurp filename))))

(defn update-map [m entries]
  (assoc m
    :format ueberfoo-format
    :updated (make-timestamp-str)
    :max-id (inc (:max-id m))
    :entries entries))

(defn add-entry [filename words]  
  (let [text (apply concat (map #(concat % " ") words))
        m    (load-ueberfoo filename)
        x    (parse-text text m)
        out  (update-map m (conj (:entries m) x))]
    (do
      (prn x)
      (spit filename out)
      (let [n (count (:entries out))]
        (if (= 1 n)
          (println "1 entry.")
          (println (str n " entries.")))))))

(defn make-post-processor [opt-map nr-of-entries]
  (let [if-reverse (if (:reverse opt-map) reverse identity)
        offset (if-let [n (:offset opt-map)] n 0)
        limit  (if-let [n (:limit opt-map)] n nr-of-entries)]
    (comp vec #(take limit (drop offset %)) if-reverse)))

(defn format-value [v]
  (if (set? v)
    (let [xs (for [x v] (if (keyword? x)
                          (str "#" (name x) " ")
                          (str x " ")))]
      (.trim (apply str xs)))
   v))

(defn make-entry-formatter [opt-map]
  (if (:clj opt-map)
    identity
    (let [make-line (fn [kv-formatter kv-pair]
                      (str (apply str (kv-formatter kv-pair) "\n"))),
          d (:display opt-map),
          kv-formatter
          (cond
           (or
            (nil? d)
            (= "kv" d)) #(str (name (get % 0)) ": " (format-value (get % 1)))
            (= "k" d)   #(str (name (get % 0)))
            (= "v" d)   #(format-value (get % 1)))]
      (fn [entry]
        (apply str (for [kv-pair entry] (make-line kv-formatter kv-pair)))))))

(defn make-selector [opt-map]
  (if (:all opt-map)
    identity
    #(select-keys % (reverse (:select opt-map)))))

(defn make-filter [opt-map]
  (let [xs (:filter opt-map)]
    (if (empty? xs)
      identity
      (let [f-txts (filter string? xs),
            f-tags (filter keyword? xs),
            f-kvs  (filter map? xs),
            all-text-tokens
            (fn [entry]
              (or (empty? f-txts)
                  (let [text (:text entry)]
                    (apply l-and (map (partial supstring-of? text) f-txts))))),
            all-tags
            (fn [entry]
              (or (empty? f-tags)
                  (let [tags (:tags entry)]
                    (apply l-and (map (partial contains? tags) f-tags))))),
            all-kvs
            (fn [entry]
              (or (empty? f-kvs)
                  (apply l-and
                         (for [e f-kvs]
                           (let [kv (first e)
                                 test-k  (get kv 0)
                                 test-v  (get kv 1)
                                 entry-v (test-k entry)]
                             (cond
                              ;; special case for "key=" (w/o value)
                              (= :any test-v) (not (nil? entry-v)) ;; then that suffices
                              (can-as-number? test-v) (and (can-as-number? entry-v)
                                                           (= (as-number test-v) (as-number entry-v)))
                              :else (substring-of? test-v entry-v)))))))]
            (fn [entry] (and (all-text-tokens entry)
                             (all-tags entry)
                             (all-kvs entry)))))))

(defn list-entries [filename & [options]]
  (let [xs (:entries (load-ueberfoo filename)),
        options-vec
        (cond
         (nil? options) []
         (coll? options) (vec options)
         (= java.lang.String (class (first options))) (vec (.split (first options) " "))
         :else (do (println "unknown class for options arg" (class options))
                   (assert false))),
        opt-map     (parse-list-options options-vec)
        fxs         (filter (make-filter opt-map) xs)
        sel         (map (make-selector opt-map) fxs)
        res         ((make-post-processor opt-map (count sel)) sel)
        eform       (make-entry-formatter opt-map)
        one-liners? (and (not (:all opt-map)) (= 1 (count (:select opt-map))))
        printer     (if one-liners? print println)]
    (do
      (doseq [r res] (printer (eform r)))
      (if (:verbose opt-map)
        (println (str (count res) "/" (count xs) " entries."))))))


;;; invocation

(defn usage []
  (println (str "USAGE:\n"
                "FILENAME new\n"
                "FILENAME add TEXT\n"
                "FILENAME list [options]")))

(defn- run-script []
  (let [[filename cmd & r] *command-line-args*]
    (case cmd
          "new" (new-file filename)
          "add" (add-entry filename r)
          "list" (list-entries filename r)
          (usage))))

(run-script)


