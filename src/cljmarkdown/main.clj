(ns cljmarkdown.main
  (:use [cljmarkdown.block :only [parse-file]]))

(defn -main
  [input-file]
  (println (parse-file input-file)))
