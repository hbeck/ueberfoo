(ns ueberfoo.common.queries)

(defn tag-count [entries]
  "map key for every tag found, value = nr of occurences, e.g.,
   {:todo 42 :done 0 :urgent 10}"
  (loop [head (first entries), tail (rest entries), res {}]
    (if (empty? head)
      res
      (let [local (apply merge (map (fn [x] {x 1}) (:tags head)))]
        (recur (first tail) (rest tail) (merge-with + res local))))))

(defn tag-rank [entries]
  "sorted vec of [:tagname nr-of-occurences]"
  (sort #(>= (get %1 1) (get %2 1))
        (map vec (tag-count entries))))