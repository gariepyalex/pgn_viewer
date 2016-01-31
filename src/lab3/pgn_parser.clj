(ns lab3.pgn-parser
  (:require [lab3.chess-board :as chess]))

(def game-without-metadata-regex #"(?:\[.*\]\s)*([^\[\]]*)")
(def move-regex #"\d+\.\s?(\S+)\s([^01]\S+)?")
(def move-detail-regex #"([KQNBR]?)([a-h]?[1-8]?)(x?)([a-h][1-8])=?([KQNBR]?)(\+?)")

(defn remove-metadata
  [pgn-string]
  (second (re-matches game-without-metadata-regex pgn-string)))

(def piece-type-strings {"K" :king
                         "Q" :queen
                         "N" :knight
                         "B" :bishop
                         "R" :rook})

(defn parse-type
  [type-string]
  (get piece-type-strings type-string))

(defn parse-move
  [move-string]
  (cond
    (= "O-O" move-string)
    {:move-type :kingside-castle}

    (= "O-O-O" move-string)
    {:move-type :queenside-castle}

    :else
    (let [[_ type from take? to promotion check?] (re-matches move-detail-regex move-string)]
      (cond-> {:move-type :move}
        (not (empty? type))      (assoc :type (parse-type type))
        (not (empty? from))      (assoc :from (keyword from))
        (not (empty? take?))      (assoc :take? true)
        (not (empty? to))        (assoc :to (keyword to))
        (not (empty? promotion)) (assoc :promotion (parse-type promotion))
        (not (empty? check?))    (assoc :check? true)))))

(defn parse-moves
  [pgn-without-metadata]
  (->> (re-seq move-regex pgn-without-metadata)
       (mapcat #(drop 1 %))
       (map parse-move)))

(defn game-state-seq
  [move-seq]
  (reduce (fn [states move] (conj states (chess/move (last states) move)))
          [chess/initial-board]
          move-seq))

(defn parse-pgn
  [pgn-string]
  (->> pgn-string
       remove-metadata
       parse-moves
       (map (fn [color move] (assoc move :color color)) (interleave (repeat :white)
                                                                    (repeat :black)))
       game-state-seq))
