(defn letter
  ([input]
   (let[[fst & more] input]
     (if fst [[fst] more] []))))

(defn digit?
  [c]
  (let [v (apply - (map int [c \0]))]
    (and (>= v 0) (<= v 9))))

(defn lit?
  [c]
  (fn [ch] (= c ch)))

(def star? (lit? \*))
(def space? (lit? \space))

(defn em
  [line]
  (loop [[fst snd & more :as raw] line output []]
    (if (empty? output) ;not meet open em
      (cond
        (some nil? [fst snd]) [nil line]
        (and
          (star? fst)
          (not (space? snd))) (recur (rest raw) (conj output fst))
        :else [nil line])
      (cond
        (some nil? [fst snd]) [nil line]
        (and
          (not (space? fst))
          (star? snd)
          (not (star? (first more)))) [(apply str (conj output fst snd)) more]
        :else (recur (rest raw) (conj output fst))))))

(defn strong
  [line]
  (loop [[fst snd thd & more :as raw] line output []]
    (if (empty? output)
      (cond
        (some nil? [fst snd thd]) [nil line]
        (and
          (every? star? [fst snd])
          (not (space? thd))) (recur (drop 2 raw) (conj output fst snd))
        :else [nil line])
      (cond
        (some nil? [fst snd thd]) [nil line]
        (and
          (every? star? [snd thd])
          (not (space? fst))
          (not (star? (first more)))) [(apply str (conj output fst snd thd)) more]
        :else (recur (rest raw) (conj output fst))))))
