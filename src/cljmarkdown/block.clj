(ns cljmarkdown.block
  (:use [cljmarkdown.helper])
  (:use [cljmarkdown.markdown :only [parse-inline]])
  (:use [clojure.string :only [join]])
  (:use [clojure.java.io]))

(defn paragraph
  ([lines]
   (paragraph lines true))
  ([lines nested]
   (loop [[[s & _ :as fst] & more :as toparse] lines output []]
     (cond
       (nil? fst) [output nil]
       (setext-heading-line? fst) (if (space-line? (last output))
                                    (recur more (conj output fst))
                                    [output (conj toparse (last output))])
       (and nested (or
                     (list-line? fst)
                     (quote? s))) [output toparse]

       (or (atx-heading-line? fst)
           (horizontal-line? fst)
           (space-line? fst)
           (hash? s)
           (quote? s)) [(conj output fst) more]
       :else (recur more (conj output fst))))))

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

(defn blockquote
  [lines]
  (loop [[line & more :as toparse] lines output []]
    (cond
      (or (horizontal-line? line)
          (and (space-line? line)
               (not (quote-line? (first more))))) [output toparse]
      :else (recur more (conj output line)))))

(defn codeblock
  [lines]
  (let [output (take-while
                 codeblock-line? lines)]
    [output (drop (count output) lines)]))

(defn list-block
  [lines]
  (loop [[fst & more :as toparse] lines output []]
    (cond
      (nil? fst) [output nil]
      (or (horizontal-line? fst)
          (codeblock-line? fst)) [output toparse]
      (or (space-line? fst)
          (list-line? fst)) (recur more (conj output fst))
      (and (not (space? (first fst)))
           (space-line? (last output))) [output toparse]
      :else (recur more (conj output fst)))))

(defn get-list-items
  [lines]
  (let [indent (count-list-indent lines)]
    (loop [[line & more] lines output [] coll []]
      (cond
        (nil? line) (conj output coll)
        (and (list-line? line)
             (= indent (count-list-indent [line]))) (if (empty? coll)
                                                      (recur more [] [line])
                                                      (recur more (conj output coll) [line]))
        :else (recur more output (conj coll line))))))

(defn parse-list [lines] nil)

(defn parse-lines'
  ([lines]
   (parse-lines' lines false))
  ([lines nested]
   (loop [[line & more :as toparse] lines output []]
     (cond
       (nil? line) output
       (codeblock-line? line) (let [[o m] (codeblock toparse)]
                                (recur m (conj output (mk-codeblock o))))
       (horizontal-line? line) (recur more (conj output (mk-hr line)))
       (atx-heading-line? line)(recur more (conj output (mk-heading line)))
       (setext-heading-line? (first more)) (recur (rest more) (conj output (mk-heading line)))
       (quote-line? line) (let [[o m] (blockquote toparse)]
                            (recur m (conj output (mk-quoteblock o))))
       (list-line? line) (let [[o m] (list-block toparse)]
                           (if (ul-marker? line)
                             (recur m (conj output (mk-ul-list o)))
                             (recur m (conj output (mk-ol-list o)))))
       :else (let [[o m] (paragraph toparse nested)]
               (recur m (conj output (mk-paragraph o))))))))

(defn parse-lines
  ([lines]
   (parse-lines lines false))
  ([lines nested]
   (defn- nested-parse
     [lines]
     (parse-lines lines true))
   (loop [[parsed & more] (parse-lines' lines nested) tree {:type :root :value []}]
     (let [t (:type parsed)]
       (cond
         (nil? parsed) tree
         (or (= :codeblock t)
             (= :paragraph t)
             (= :heading t)
             (= :hr t)) (recur more (add-child tree parsed))

         (or (= :ul t)
             (= :ol t)) (recur more (add-child
                                      tree
                                      (assoc parsed :value
                                             (parse-list (:value parsed) ))))
         (= :quoteblock t) (recur more (add-child
                                         tree
                                         (assoc parsed :value
                                                (:value (-> (:value parsed) quote-inner-text nested-parse)))))
         :else (recur more (assoc tree :value
                                  (conj (:value tree) parsed))))))))

(defn parse-list
  [lines]
  (defn- update-type
    [node]
    (assoc node :type :li))
  (defn- nested-parse
    [lines]
    (parse-lines lines true))
  (let [items (get-list-items lines)]
    (map #(update-type (-> % extract-text-from-li nested-parse)) items)))



(defn- render'
  [node html]
  (let [t (:type node)]
    (cond
      (= :hr t) "<hr/>"
      (empty? html) html
      (= :root t) html
      (= :heading t) (str "<h1>" html "</h1>")
      (= :paragraph t) (str "<p>" html "</p>")
      (= :quoteblock t) (str "<blockquote>\n" html "\n</blockquote>")
      (= :codeblock t) (str "<pre><code>\n" html "\n</code></pre>")
      (= :ol t) (str "<ol>\n" html "\n</ol>")
      (= :ul t) (str "<ul>\n" html "\n</ul>")
      (= :li t) (str "<li>" html "</li>"))))

(defn render
  [node]
  (let [t (:type node) v (:value node)]
    (cond
      (= t :paragraph) (render' node
                               (apply str 
                                      (join \newline
                                            (map parse-inline v))))
      (= :heading t) (render' node (apply str
                                          (parse-inline v)))
      (or (= :codeblock t)
          (= :hr t)) (render' node (apply str (join \newline v)))
      :else (let [html (apply str (join \newline (map render v)))]
              (render' node html)))))

(defn parse-file
  [file]
  (with-open [rdr (reader file)]
    (render (parse-lines (line-seq rdr)))))
