;; This is a personal practice project, to drive me to learn clojure. ;-)
(defn
  count-leading-hash
  [line]
  (count (take-while #(= % \#) (take 6 line))))

(defn
  drop-trailing-chars
  [s chars]
  (loop [line s]
    (if (some #(= % (last line)) chars) (recur (butlast line)) line)))

(defn
  ^{:doc "Process <h[1-6]> elements"}
  process-heading
  ([line]
    (let [lh (count-leading-hash line)
          head (drop-trailing-chars (drop lh line) "\n #")]
      (format "<h%s>%s</h%s>" lh (apply str head) lh))))

(defn
  group-bold
  [line]
  (loop [idx 0 [c & m] line prev [nil nil] groups []]
    (case c
        \* (if (even? (count groups)); even indexes?
              (if (and (= \* (first m)) (not= \space (first (rest m))));
                (recur (+ 2 idx) (rest m) (conj (vec (rest prev)) (first m)) (conj groups idx))
                (recur (inc idx) m (conj (vec (rest prev)) c) groups))
              (if (and (= \* (last prev)) (not= \space (first prev)) (not= \* (first m)))
                (recur (inc idx) m (conj (vec (rest prev)) c) (conj groups idx))
                (recur (inc idx) m (conj (vec (rest prev)) c) groups)))
        nil (if (even? (count groups)) groups (vec (butlast groups)))
        (recur (inc idx) m (conj (vec (rest prev)) c) groups))))
