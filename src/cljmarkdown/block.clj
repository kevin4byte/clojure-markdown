(ns cljmarkdown.block
  (:use [cljmarkdown.helper]))

(defn paragraph
  [lines]
  (loop [[[s & _ :as fst] & more :as toparse] lines output []]
    (cond
      (nil? fst) [output nil]
      (setext-heading-line? fst) (if (space-line? (last output))
                            (recur more (conj output fst))
                            [output (conj toparse (last output))])
      (or (atx-heading-line? fst)
          (horizontal-line? fst)
          (hash? s)
          (quote? s)) [output toparse]
      :else (recur more (conj output fst)))))

(defn heading
  [lines]
  (let [[fst snd & _] lines]
    (cond
      (setext-heading-line? snd) [fst (-> lines rest rest)]
      (atx-heading-line? fst) [fst (rest lines)]
      :else [nil lines])))

(defn horizontal
  [lines]
  (let [[fst & more] lines]
    (if (horizontal-line? fst)
      [fst more]
      [nil lines])))
