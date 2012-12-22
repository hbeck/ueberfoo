(ns ueberfoo.common.filter
  (:use [ueberfoo.common common strings]))

(defn find-first [pred coll]
  (loop [x (first coll), xs (rest coll)]
    (cond
     (empty? x) nil
     (pred x) x
     :else (recur (first xs) (rest xs)))))

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
