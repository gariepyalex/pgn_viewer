(ns lab3.chess-board
  (:require [clojure.set :as set]))

(def initial-board {:a1 {:color :white :type :rook}
                    :b1 {:color :white :type :knight}
                    :c1 {:color :white :type :bishop}
                    :d1 {:color :white :type :queen}
                    :e1 {:color :white :type :king} :f1 {:color :white :type :bishop} :g1 {:color :white :type :knight}
                    :h1 {:color :white :type :rook}
                    :a2 {:color :white :type :pawn}
                    :b2 {:color :white :type :pawn}
                    :c2 {:color :white :type :pawn}
                    :d2 {:color :white :type :pawn}
                    :e2 {:color :white :type :pawn}
                    :f2 {:color :white :type :pawn}
                    :g2 {:color :white :type :pawn}
                    :h2 {:color :white :type :pawn}
                    :a3 nil :b3 nil :c3 nil :d3 nil :e3 nil :f3 nil :g3 nil :h3 nil
                    :a4 nil :b4 nil :c4 nil :d4 nil :e4 nil :f4 nil :g4 nil :h4 nil
                    :a5 nil :b5 nil :c5 nil :d5 nil :e5 nil :f5 nil :g5 nil :h5 nil
                    :a6 nil :b6 nil :c6 nil :d6 nil :e6 nil :f6 nil :g6 nil :h6 nil
                    :a7 {:color :black :type :pawn}
                    :b7 {:color :black :type :pawn}
                    :c7 {:color :black :type :pawn}
                    :d7 {:color :black :type :pawn}
                    :e7 {:color :black :type :pawn}
                    :f7 {:color :black :type :pawn}
                    :g7 {:color :black :type :pawn}
                    :h7 {:color :black :type :pawn}
                    :a8 {:color :black :type :rook}
                    :b8 {:color :black :type :knight}
                    :c8 {:color :black :type :bishop}
                    :d8 {:color :black :type :queen}
                    :e8 {:color :black :type :king}
                    :f8 {:color :black :type :bishop}
                    :g8 {:color :black :type :knight}
                    :h8 {:color :black :type :rook}})

(def pos->coordinate (into {}
                           (for [i (range 1 9)
                                 j (range 1 9)]
                             [(keyword (str (get "abcdefgh" (dec j)) i)) [i j]])))

(def coordinate->pos (set/map-invert pos->coordinate))

(def valid-positions (set (keys pos->coordinate)))

(def column-names (set (map #(keyword (str %)) "abcdefg")))

(defn in-column?
  [pos column]
  (= (first (name pos)) (first (name column))))

(defn pawn-in-home-row?
  [pos color]
  (let [row-number (-> pos name second str Integer/parseInt)]
    (or (and (= :black color) (= 7 row-number))
        (and (= :white color) (= 2 row-number)))))

(defn find-pieces
  [board & {:keys [color type]}]
  (let [color-filter-fn (if color (fn [[pos val]] (= color (:color val))) identity)
        type-filter-fn  (if type (fn [[pos val]] (= type (:type val))) identity)]
    (->> board
         (filter color-filter-fn)
         (filter type-filter-fn))))

(defn diagonal-move?
  [from-pos to-pos]
  (let [from-coord (from-pos pos->coordinate)
        to-coord (to-pos pos->coordinate)]
    (= (Math/abs (- (first from-coord) (first to-coord)))
       (Math/abs (- (second from-coord) (second to-coord))))))

(defn straight-move?
  [from-pos to-pos]
  (let [from-coord (from-pos pos->coordinate)
        to-coord (to-pos pos->coordinate)]
    (or (= (first from-coord) (first to-coord))
        (= (second from-coord) (second to-coord)))))

(defn adjacent?
  [from-pos to-pos]
  (let [from-coord (from-pos pos->coordinate)
        to-coord (to-pos pos->coordinate)]
    (= 1 (max (Math/abs (- (first from-coord) (first to-coord)))
              (Math/abs (- (second from-coord) (second to-coord)))))))

(defn possible-knight-moves
  [from-pos]
  (let [from-coord         (from-pos pos->coordinate)
        possible-movements [[2 1] [2 -1] [-2 1] [-2 -1] [1 2] [1 -2] [-1 2] [-1 -2]]
        possible-to-pos    (for [movement possible-movements]
                             (into [] (map #(+ %1 %2) from-coord movement)))]
    (->> possible-to-pos
         (map #(get coordinate->pos %))
         (filter #(not (nil? %)))
         (into #{}))))

(defmulti can-move-to?
  "
  Here we don't actually check chess rules, because we assume the pgn file is value.
  We only chack for movements.
  "
  (fn [[pos piece] to take?] (:type piece)))

(defmethod can-move-to?
  :rook
  [[from-pos piece] to-pos take?]
  (straight-move? from-pos to-pos))

(defmethod can-move-to?
  :bishop
  [[from-pos piece] to-pos take?]
  (diagonal-move? from-pos to-pos))

(defmethod can-move-to?
  :knight
  [[from-pos piece] to-pos take?]
  (contains? (possible-knight-moves from-pos) to-pos))

(defmethod can-move-to?
  :king
  [[from-pos piece] to-pos take?]
  (and
   (adjacent? from-pos to-pos)
   (or (straight-move? from-pos to-pos)
       (diagonal-move? from-pos to-pos))))

(defmethod can-move-to?
  :queen
  [[from-pos piece] to-pos take?]
  (or (straight-move? from-pos to-pos)
      (diagonal-move? from-pos to-pos)))

(defmethod can-move-to?
  :pawn
  [[from-pos piece] to-pos take?]
  (let [[from-i from-j] (from-pos pos->coordinate)
        [to-i to-j]     (to-pos pos->coordinate)
        move-i          (- to-i from-i)
        move-j          (- to-j from-j)
        color           (:color piece)
        from-home-row?  (pawn-in-home-row? from-pos color)]
    (and (or (and (= :white color) (or (= 1 move-i) (and from-home-row? (= 2 move-i))))
             (and (= :black color) (or (= -1 move-i) (and from-home-row? (= -2 move-i)))))
         (or (and take? (= 1 (Math/abs move-j)))
             (and (not take?) (zero? move-j))))))

(defn piece-that-moves
  [board color type to take? & [from-column]]
  (let [potential-pieces (find-pieces board :color color :type type)
        filter-column-fn (if from-column
                           (fn [[pos piece]] (in-column? pos from-column))
                           identity)]
    (->> potential-pieces
         (filter filter-column-fn)
         (filter #(can-move-to? % to take?))
         first)))

(defmulti move
  (fn [board move] (:move-type move)))

(defmethod move
  :kingside-castle
  [board {:keys [color]}]
  (if (= :white color)
    (-> board
        (assoc :h1 nil)
        (assoc :e1 nil)
        (assoc :g1 {:type :king :color :white})
        (assoc :f1 {:type :rook :color :white}))
    (-> board
        (assoc :h8 nil)
        (assoc :e8 nil)
        (assoc :g8 {:type :king :color :black})
        (assoc :f8 {:type :rook :color :black}))))

(defmethod move
  :queenside-castle
  [board {:keys [color]}]
  (if (= :white color)
    (-> board
        (assoc :a1 nil)
        (assoc :e1 nil)
        (assoc :c1 {:type :king :color :white})
        (assoc :d1 {:type :rook :color :white}))
    (-> board
        (assoc :a8 nil)
        (assoc :e8 nil)
        (assoc :c8 {:type :king :color :black})
        (assoc :d8 {:type :rook :color :black}))))

(defmethod move
  :move
  [board {:as move :keys [color type to from take? promotion] :or {type :pawn}}]
  (let [[from piece] (if (contains? valid-positions from)
                       [from (get board from)]
                       (piece-that-moves board color type to take? (when (contains? column-names from) from)))]
      (-> board
          (assoc from nil)
          (assoc to piece)
          (#(if promotion
              (assoc-in % [to :type] promotion)
              %)))))
