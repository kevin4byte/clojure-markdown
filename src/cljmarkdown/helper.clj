(ns cljmarkdown.helper)

(defn digit?
  [c]
  (let [v (apply - (map int [c \0]))]
    (and (>= v 0) (<= v 9))))

(defn lit?
  [c]
  (fn [ch] (= c ch)))

(def asterisk? (lit? \*))
(def space? (lit? \space))
(def escape? (lit? \\))
(defn need-escape?
  [c]
  (some #((lit? c) %)
        (seq "\\`*_{}[]()#+-.!")))

