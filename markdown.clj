(defn
	process-inline
	([line] 
		(loop [c (first line) prev nil other (rest line)]
			(case c
				\# (println (str "Enter heading, " (= prev \newline)))
				\* (println "strong")
				(println (str "normal char, " c)))
			(if (not (nil? (first other))) (recur (first other) c (rest other))))))

(defn trip-car)


(defn
  count-leading-hash
  [line]
  (let [cnt (count (take-while #(= % \#) line))]
    (if (> cnt 6) 6 cnt)))

(defn
  ^{:doc "Process <h[1-6]> elements"}
  process-head
  ([line]
    (let [lh (count-leading-hash line)]
    (str "<h" lh ">" (apply str (reverse
     (drop-while
      #(= % \#) 
      (drop 1
        (reverse 
          (drop-while
           #(= % \#) 
           line)))))) "</h" lh ">"))))