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

(defn setext-heading-line?
  [line]
  (every? heading? line))

(defn atx-heading-line?
  [line]
  (hash? (first line)))

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

(defn horizontal-line?
  [line]
  (some #((with-space %) (remove-line-break line))(seq "*-_")))
