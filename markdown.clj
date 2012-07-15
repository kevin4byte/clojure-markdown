;; This is a personal practice project, to drive me to learn clojure. ;-)
(defn
  count-leading-hash
  [line]
  (let [cnt (count (take-while #(= % \#) line))]
    (if (> cnt 6) 6 cnt)))

(defn
  drop-trailing-chars
  [s chars]
  (loop [end (last s) ln (drop-last s)]
    (if (> (.indexOf chars (str end)) -1)
      (recur (last ln) (drop-last ln))
      (concat ln `(~end)))))

(defn
  ^{:doc "Process <h[1-6]> elements"}
  process-heading
  ([line]
    (let [lh (count-leading-hash line)
          head (drop-trailing-chars (drop lh line) "\n #")]
      (format "<h%s>%s</h%s>" lh (apply str head) lh))))

(defn
  process-close-italic
  [content]
  (loop [init [] [fst & more] content]
    (case fst
      \* (if 
        (and 
          (not= \* (first more)) ; next char is not *.
                                 ; *aa**bb**cc* should be translate to <em>aa<strong>bb</strong>cc</em>
          (not= \space (last init))); previous char is not space, *a * is not a valid input.
           [true init] (recur (conj init fst) more))
      (if (nil? fst) [false init] (recur (conj init fst) more)))))

(defn
  process-bold
  []
  ())

(defn
  process-italic
  [content]
  (let [[c & more] content]
    (case c
      \* (process-bold more)
      \space content ;list item should be hanndled at start of processing a line
      (process-close-italic more))))

(defn
  process-inline
  [content]
  (loop [fst (first content) remain (rest content)]
    (case fst
      \* (2)
      \_ (3)
      (4))))