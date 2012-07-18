;; This is a personal practice project, to drive me to learn clojure. ;-)
(defn count-leading-hash
  [line]
  (count (take-while #(= % \#) (take 6 line))))

(defn drop-trailing-chars
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

(defn- find-open-bold
  [idx line]
  (let [[head & more] line
        open-bold? #(and (= (first %) \*) (not= (first (rest %)) \space))]
    (cond
      (nil? head) nil
      (= \* head) (if (open-bold? more) [idx (rest more)] (recur (inc idx) more))
      :else (recur (inc idx) more))))

(defn- find-close-bold
  [idx line]
  (let [[head & more] line
        close-bold? #(and (= (take 2 %) '(\* \*)) (not= (first (nthrest % 2)) \*))]
    (cond
      (nil? head) nil
      (not= \space head) (if (close-bold? more) [(inc idx) (nthrest more 2)] (recur (inc idx) more))
      :else (recur (inc idx) more))))

(defn- find-bold
  [offset line]
  (if-let [[open-pos more] (find-open-bold 0 line)]
    (if-let [[close-pos more] (find-close-bold 0 more)]
      [[(+ offset open-pos) (+ offset open-pos close-pos 2)] more])))

(defn find-bolds
  [line]
  (loop [groups []
         line line
         offset 0]
    (cond
      (empty? line) groups
      :else (let [[group more :as bold] (find-bold offset line)]
        (if (nil? bold) 
          groups
          (recur (conj groups group) more (+ 2 (last group))))))))
;;
;; find-bolds and group-bolds are functional indentical. which is better?
;;
(defn
  group-bolds
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

(defn- find-open-em
  [idx line]
  (let [[fst & m] line
        open-em? #(not= (first %) \space)]
    (cond
      (nil? fst) nil
      (= fst \*) (if (open-em? m) [idx m] (recur (inc idx) m))
      :else (recur (inc idx) m))))

(defn- find-close-em
  [idx line]
  (let [[fst & m] line
        close-em? #(= (first %) \*)]
    (cond
      (nil? fst) nil
      (not= fst \space) (if (close-em? m) [(inc idx) (rest m)] (recur (inc idx) m))
      :else (recur (inc idx) m))))

(defn- find-em
  [offset line]
  (if-let [[open-em more] (find-open-em 0 line)]
    (if-let [[close-em more] (find-close-em 0 more)]
      [[(+ offset open-em) (+ offset close-em open-em 1)] more])))

(defn- find-ems
  [line]
  (loop [groups []
         offset 0
         line line]
    (cond
      (empty? line) groups
      :else (let [[group more :as em] (find-em offset line)]
        (if (nil? em)
          groups
          (recur (conj groups group) (+ 1 (last group)) more))))))