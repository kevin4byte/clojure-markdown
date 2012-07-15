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

