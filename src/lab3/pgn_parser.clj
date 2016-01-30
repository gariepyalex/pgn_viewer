(ns lab3.pgn-parser)

(def game-without-metadata-regex #"(?:\[.*\]\s)*([^\[\]]*)")
(def move-regex #"(\d+)\.\s?(\S+)\s([^01]\S+)?")

(defn remove-metadata
  [pgn-string]
  (second (re-matches game-without-metadata-regex pgn-string)))
