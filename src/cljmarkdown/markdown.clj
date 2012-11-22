(ns cljmarkdown.markdown
  (:use [cljmarkdown.helper]))


(defn em
  "Parse an <em> elements, the line should start with *."
  [line]
  (loop [[fst snd & more :as raw] line output []]
    (if (empty? output) ;not meet open em yet
      (cond
        (some nil? [fst snd]) [nil line]
        (and
          (asterisk? fst)
          (not (space? snd))) (recur (rest raw) (conj output fst))
        :else [nil line])
      (cond
        (nil? fst) [nil line]
        (escape? fst) (let [[esc r] (escape raw)]
                        (let [[f s & m] r]
                          (if (and (not (asterisk? s)) (asterisk? f))
                            [(apply str (conj output esc f)) (rest r)]
                            (recur r (conj output esc)))))
        (and
          (not (space? fst))
          (asterisk? snd)
          (not (asterisk? (first more)))) [(apply str (conj output fst snd)) more]
        :else (recur (rest raw) (conj output fst))))))

(defn strong
  [line]
  (loop [[fst snd thd & more :as raw] line output []]
    (if (empty? output)
      (cond
        (some nil? [fst snd thd]) [nil line]
        (and
          (every? asterisk? [fst snd])
          (not (space? thd))) (recur (drop 2 raw) (conj output fst snd))
        :else [nil line])
      (cond
        (some nil? [fst snd]) [nil line]
        (escape? fst) (let [[esc other] (escape raw)]
                        (let [[ f s & m] other]
                          (if (and (every? asterisk? [f s]) (not (asterisk? m)))
                            [(apply str (conj output esc f s)) m]
                            (recur other (conj output esc)))))
        (and
          (every? asterisk? [snd thd])
          (not (space? fst))
          (not (asterisk? (first more)))) [(apply str (conj output fst snd thd)) more]
        :else (recur (rest raw) (conj output fst))))))


(defn- hyper-link-url
  [line]
  (loop [[fst & more :as input] line output []]
    (if (empty? output)
      (cond
        (nil? fst) [nil line]
        (open-parenthesis? fst) (recur more (conj output fst))
        :else [nil line])
      (cond
        (nil? fst) [nil line]
        (escape? fst) (let [[esc other] (escape input)]
                        (recur other (conj output esc)))
        (close-parenthesis? fst) [(apply str (conj output fst)) more]
        :else (recur more (conj output fst))))))

(defn hyper-link
  [line]
  (loop [[fst & more :as input] line output [] unmatch 0]
    (if (empty? output)
      (cond
        (open-bracket? fst) (recur more (conj output fst) 1)
        :else [nil line])
      (cond
        (nil? fst) [nil line]
        (escape? fst) (let [[esc other] (escape input)]
                        (recur other (conj output esc) unmatch))
        (open-bracket? fst) (recur more (conj output fst) (+ unmatch 1))
        (close-bracket? fst) (if (and (zero? (- unmatch 1)) (open-parenthesis? (first more)))
                               (let [op (hyper-link-url more)]
                                 (if (nil? (first op))
                                   [nil line]
                                   [(apply str (conj output fst (first op))) (last op)]))
                               (recur more (conj output fst) (- unmatch 1)))
        :else (recur more (conj output fst) unmatch)))))



(defn- dump [txt] (apply str txt))
(defn- parse-line'
  [line]
  (loop [[fst & more :as input] line output [] text []]
    (cond
      (nil? fst) [(conj output (mk-text (dump text))) nil]
      (escape? fst) (let [[esc more] (escape input)]
                      (recur more output (conj text esc)))
      (asterisk? fst) (let [[p r] (strong input)]
                        (if (nil? p)
                          (let [[p1 r1] (em input)]
                            (if (nil? p1)
                              (recur more output (conj text fst))
                              (recur r1 (conj output (-> text dump mk-text) (mk-em p1)) [])))
                          (recur r (conj output (-> text dump mk-text) (mk-strong p)) [])))
      (open-bracket? fst) (let [[p r] (hyper-link input)]
                            (if (nil? p)
                              (recur more output (conj text fst))
                              (recur r (conj output (-> text dump mk-text) (mk-hyper-link p)) [])))
      :else (recur more output (conj text fst)))))

(defn parse-line
  [line]
  (let [[eles & _] (parse-line' line)]
    (loop [[fst & more] eles tree {:type :root :value []}]
      (cond
        (nil? fst) tree
        (= :hyperlink (:type fst)) (recur more (add-child tree (assoc fst
                                                                      :value (:value
                                                                         (parse-line
                                                                           (extract-link-text
                                                                             (:value fst)))))))
        (= :strong (:type fst)) (recur more (add-child tree (assoc fst
                                                                   :value (:value
                                                                            (parse-line
                                                                              (extract-strong-text
                                                                                (:value fst)))))))
        (= :em (:type fst)) (recur more (add-child tree (assoc fst
                                                               :value (extract-em-text
                                                                                   (:value fst)))))
        :else (recur more (add-child tree fst))))))
(defn escape'
  [text]
  (loop [[fst snd & more :as input] text output []]
    (cond
      (some nil? [fst snd]) (apply str (conj output (apply str input)))
      (and (escape? fst) (need-escape? snd)) (recur more (conj output snd))
      :else (recur (rest input) (conj output fst)))))

(defn- render'
  [node html]
  (let [t (:type node)]
    (cond
      (= :strong t) (str "<strong>" html "</strong>")
      (= :em t) (str "<em>" html "</em>")
      (= :hyperlink t) (str "<a href=\"" (:url node) "\">" html "</a>")
      :else html)))

(defn render
  [node]
  (if (string? (:value node))
    (render' node (escape' (:value node)))
    (let [html (reduce str "" (map render (:value node)))]
      (render' node html))))

(defn parse-inline
  [text]
  (render (parse-line text)))
