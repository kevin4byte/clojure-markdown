(ns cljmarkdown.helper)

(defn digit?
  [c]
  (let [v (apply - (map int [c \0]))]
    (and (>= v 0) (<= v 9))))

(defn lit?
  [c]
  (fn [ch] (= c ch)))

(def asterisk? (lit? \*))
(def escape? (lit? \\))
(def dot? (lit? \.))

(defn need-escape?
  [c]
  (some #((lit? c) %)
        (seq "\\`*_{}[]()#+-.!")))

(defn space?
  [c]
  (some #(= c %) (seq "\r\n\t ")))

(def hash? (lit? \#))
(def quote? (lit? \>))

(defn space-line?
  [line]
  (every? space? line))

(defn heading?
  [c]
  (some #(= c %) (seq "=-")))

(defn quote-line?
  [line]
  (quote? (first (drop-while space? line))))

(defn setext-heading-line?
  [line]
  (and (not (nil? line))
       (not (empty? line))
       (every? heading? line)))

(defn atx-heading-line?
  [line]
  (hash? (first line)))

(defn heading-line?
  [line]
  (or (atx-heading-line? line)
      (setext-heading-line? line)))

(defn escape
  [line]
  (let [[fst snd & more] line]
    (cond
      (and (need-escape? snd) (escape? fst)) [(str fst snd) more]
      (nil? snd) [fst more]
      :else [[] line])))

(defn- with-space
  [c]
  (defn- space-or-char?
    [x]
    (some #(= x %) [c \space]))

  (fn [line]
    (every? space-or-char? line)))

(defn- remove-line-break
  [line]
  (let [last-c (last line)]
    (if (= \newline last-c)
      (let [last-c' (last (butlast  line))]
        (if (= \return last-c')
          (-> line butlast butlast)
          (-> line butlast)))
      line)))

(def open-bracket? (lit? \[))
(def close-bracket? (lit? \]))
(def open-parenthesis? (lit? \())
(def close-parenthesis? (lit? \)))

(defn extract-link-text
  [line]
  (loop [[fst & more :as input] line output [] unmatch 0]
    (if (empty? output)
      (cond
        (open-bracket? fst) (recur more (conj output fst) 1)
        :else nil)
      (cond
        (nil? fst) nil
        (escape? fst) (let [[esc other] (escape input)]
                        (recur other (conj output esc) unmatch))
        (open-bracket? fst) (recur more (conj output fst) (+ unmatch 1))
        (close-bracket? fst) (if (and (zero? (- unmatch 1)) (open-parenthesis? (first more)))
                               (apply str (rest output))
                               (recur more (conj output fst) (- unmatch 1)))
        :else (recur more (conj output fst) unmatch)))))


(defn extract-link-url
  [link]
  (let [url (drop (+ 2 (count (extract-link-text link))) link)]
    (apply str(-> url rest drop-last))))

(defn extract-strong-text
  [text]
  (drop-last 2 (drop 2 text)))

(defn extract-em-text
  [text]
  (apply str (-> text rest drop-last)))


(defn horizontal-line?
  [line]
  (and (not (space-line? line))
       (some #((with-space %) (remove-line-break line))(seq "*-_"))))

(defn- indent-with?
  [c n]
  (fn [line]
    (= (take n line) (repeat n c))))

(defn codeblock-line?
  [line]
  (some #((apply indent-with? %) line)
        [[\tab 1]
         [\space 4]]))

(defn ul-marker?
  [line]
  (let [[fst snd & _] (drop-while space? line)]
    (and (space? snd)
         (some #(= % fst) "+-*"))))

(defn- ol-marker?
  [line]
  (let [[fst snd & _ ] (drop-while digit? line)]
    (and (dot? fst)
         (not= (first line) fst)
         (space? snd))))

(defn list-line?
  [line]
  (let [trimed (drop-while space? line)]
    (or (ul-marker? trimed)
        (ol-marker? trimed))))

(defn- mk-element
  [ele etype]
  {:type etype :value ele})

(def mk-em
  (fn [value] (mk-element value :em)))

(def mk-strong
  (fn [value] (mk-element value :strong)))

(def mk-hyper-link
  (fn [value]
    (assoc (mk-element value :hyperlink)
           :url (extract-link-url value))))

(def mk-text
  (fn [value] (mk-element value :text)))

(def mk-paragraph
  (fn [lines] (mk-element lines :paragraph)))

(def mk-heading
  (fn [lines] (mk-element lines :heading)))

(def mk-ul-list
  (fn [lines] (mk-element lines :ul)))

(def mk-ol-list
  (fn [lines] (mk-element lines :ol)))

(def mk-hr
  (fn [line] (mk-element nil :hr)))

(def mk-codeblock
  (fn [lines] (mk-element lines :codeblock)))

(def mk-quoteblock
  (fn [lines] (mk-element lines :quoteblock)))

(defn count-list-indent
  [lines]
  (count
    (take-while space? (first (drop-while space-line? lines)))))


(defn add-child
  [tree node]
  (assoc tree :value (conj (:value tree) node)))

(defn extract-text-from-li
  [lines]
  (defn- drop-markers
    [line indent]
    (if (every? space? (take (+ 1 indent) line))
      (apply str (drop indent line))
      (apply str (drop-while #(or (space? %)
                                  (digit? %)) line))))
  (let [indent (count-list-indent lines)
        trimed (map #(if (list-line? %)
                       (drop-markers % indent) %) lines)]
    (cons (apply str (rest (first trimed))) (rest trimed))))

(defn quote-inner-text
  [lines]
  (defn- remove-quote
    [line]
    (let [[fst & more] (drop-while space? line)]
      (if (quote? fst)
        (apply str more)
        line)))
  (map remove-quote lines))
