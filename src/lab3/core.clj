(ns lab3.core
  (:require [clojure.java.io :as io]
            [lab3.pgn-parser :as parser]
            [lab3.game-display :as display]))

(def default-game (io/resource "byrne_fischer_1956.pgn"))

(defn -main
  [& [path]]
  (let [file-path  (or path default-game)
        pgn-string (slurp (io/file file-path))]
    (-> pgn-string
        parser/parse-pgn
        display/show-game)))
