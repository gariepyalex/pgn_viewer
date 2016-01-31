(ns lab3.game-display
  (:require [quil.core :as q]
            [quil.middleware :as m]))

(def canvas-size 500)
(def frame-rate 1)
(def chessboard-square-size (* canvas-size 1/9))

(defn line-number-to-y
  [line-number]
  (* (- 8 line-number) chessboard-square-size))

(def letter-column-index {\a 0, \b 1, \c 2, \d 3, \e 4, \f 5, \g 6, \h 7})
(defn column-letter-to-x
  [column-letter]
  (* (get letter-column-index column-letter) chessboard-square-size))

(defn pos-to-x-y
  "transform a position keyword in chess notation (e.g. :b4) to [x y]"
  [pos]
  (let [pos-string    (name pos)
        column-letter (first pos-string)
        line-number   (-> pos-string second str Integer/parseInt)]
    [(column-letter-to-x column-letter) (line-number-to-y line-number)]))

(defn setup-state
  [chess-game]
  (q/frame-rate frame-rate)
  {:game chess-game})

(defmulti draw-piece
  "We assume the piece size must be one because of prior scaling"
  (fn [piece] (:type piece)))

(defmethod draw-piece
  :default
  [piece]
  (q/rect 0 0 1 1))

(defmethod draw-piece
  :pawn
  [_]
  (q/triangle 1/2 1/3 1/5 4/5 4/5 4/5)
  (q/ellipse 1/2 1/3 1/3 1/3))

(defmethod draw-piece
  :bishop
  [_]
  (q/triangle 1/2 1/3 1/5 4/5 4/5 4/5)
  (q/begin-shape)
  (q/vertex 1/2 1/6)
  (q/vertex 5/7 2/6)
  (q/vertex 4/7 3/6)
  (q/vertex 3/7 3/6)
  (q/vertex 2/7 2/6)
  (q/vertex 1/2 1/6)
  (q/end-shape)
  (q/ellipse 1/2 1/3 1/8 1/8))

(defmethod draw-piece
  :queen
  [_]
  (q/triangle 1/2 1/3 1/5 4/5 4/5 4/5)
  (q/begin-shape)
  (q/vertex 1/2 1/6)
  (q/vertex 5/7 2/6)
  (q/vertex 4/7 3/6)
  (q/vertex 3/7 3/6)
  (q/vertex 2/7 2/6)
  (q/vertex 1/2 1/6)
  (q/end-shape)
  (q/begin-shape)
  (q/vertex 0.1 0.1)
  (q/vertex 0.9 0.1)
  (q/vertex 0.8 0.3)
  (q/vertex 0.2 0.3)
  (q/vertex 0.1 0.1)
  (q/end-shape))

(defmethod draw-piece
  :king
  [_]
  (q/triangle 1/2 1/3 1/5 4/5 4/5 4/5)
  (q/begin-shape)
  (q/vertex 0.15 0.15)
  (q/vertex 0.25 0.2)
  (q/vertex 0.35 0.15)
  (q/vertex 0.55 0.2)
  (q/vertex 0.65 0.15)
  (q/vertex 0.75 0.2)
  (q/vertex 0.85 0.15)
  (q/vertex 0.75 0.5)
  (q/vertex 0.25 0.5)
  (q/vertex 0.15 0.15)
  (q/end-shape))

(defmethod draw-piece
  :rook
  [_]
  (q/rect 0.15 0.2 0.65 0.6)
  (q/rect 0.10 0.7 0.75 0.2)
  (doseq [x (range 0.1 1 0.3)]
    (q/rect x 0.08 0.15 0.2)))

(defmethod draw-piece
  :knight
  [_]
  (q/begin-shape)
  (q/vertex 0.1 0.15)
  (q/vertex 0.7 0.2)
  (q/vertex 0.7 0.6)
  (q/vertex 0.9 0.9)
  (q/vertex 0.1 0.9)
  (q/vertex 0.2 0.8)
  (q/vertex 0.4 0.5)
  (q/vertex 0.1 0.5)
  (q/vertex 0.1 0.15)
  (q/end-shape))

(defn draw-pieces
  [game-state]
  (doseq [[pos piece] game-state]
    (let [[x y] (pos-to-x-y pos)]
      (q/translate x y)
      (q/scale chessboard-square-size)
      (q/stroke-weight 0.06)
      (cond
        (= :white (:color piece)) (do (q/fill 255 255 255) (q/stroke 0 0 0))
        (= :black (:color piece)) (do (q/fill 0 0 0) (q/stroke 255 255 255)))
      (when piece
        (draw-piece piece))
      (q/reset-matrix))))

(defn draw-background
  []
  (q/background 200 45 29)
  (q/no-stroke)
  (q/text-size 30)
  (q/fill 255 255 255)
  ;; Draw row number
  (let [x (- canvas-size (/ chessboard-square-size 2))]
    (doseq [i (range 1 9)]
      (let [y (* chessboard-square-size (+ 0.5 (- 8 i)))]
        (q/text (str i) x y))))
  ;; Draw column letter
  (let [y (- canvas-size (/ chessboard-square-size 2))]
    (doseq [j (range 8)]
      (let [x      (* chessboard-square-size (+ 0.5  j))
            letter (nth "abcdefgh" j)]
        (q/text (str letter) x y))))
  ;; Draw chessboard
  (doseq [i (range 8) j (range 8)]
    (let [x (* chessboard-square-size j)
          y (* chessboard-square-size i)]
      (if (even? (+ i j))
        (q/fill 200 200 200)
        (q/fill 50 50 50))
      (q/rect x y chessboard-square-size chessboard-square-size))))

(defn draw-state
  [state]
  (draw-background)
  (draw-pieces (first (:game state))))

(defn update-state
  [state]
  (update state :game #(concat (rest %) [(first %)])))

(defn show-game
  [game-state-seq]
  (q/sketch
   :size [canvas-size canvas-size]
   :title "Chess viewer"
   :setup (partial setup-state game-state-seq)
   :draw draw-state
   :update update-state
   :middleware [m/fun-mode]))
