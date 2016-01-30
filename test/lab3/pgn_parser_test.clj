(ns lab3.pgn-parser-test
  (:require [lab3.pgn-parser :refer :all]
            [clojure.test :refer :all]
            [clojure.string :as string]))

(def example-pgn
  (string/join "\n"
               ["[Event \"Match\"]"
                "[Site \"Philadelphia, PA USA\"]"
                "[Date \"1996.02.16\"]"
                "[EventDate \"?\"]"
                "[Round \"5\"]"
                "[Result \"0-1\"]"
                "[White \"Deep Blue (Computer)\"]"
                "[Black \"Garry Kasparov\"]"
                "[ECO \"C47\"]"
                "[WhiteElo \"?\"]"
                "[BlackElo \"?\"]"
                "[PlyCount \"94\"]"
                ""
                "1.e4 e5 2.Nf3 Nf6 3.Nc3 Nc6 4.d4 exd4 5.Nxd4 Bb4 6.Nxc6 bxc6"
                "7.Bd3 d5 8.exd5 cxd5 9.O-O O-O 10.Bg5 c6 11.Qf3 Be7 12.Rae1"
                "Re8 13.Ne2 h6 14.Bf4 Bd6 15.Nd4 Bg4 16.Qg3 Bxf4 17.Qxf4 Qb6"
                "18.c4 Bd7 19.cxd5 cxd5 20.Rxe8+ Rxe8 21.Qd2 Ne4 22.Bxe4 dxe4"
                "23.b3 Rd8 24.Qc3 f5 25.Rd1 Be6 26.Qe3 Bf7 27.Qc3 f4 28.Rd2 Qf6"
                "29.g3 Rd5 30.a3 Kh7 31.Kg2 Qe5 32.f3 e3 33.Rd3 e2 34.gxf4 e1=Q"
                "35.fxe5 Qxc3 36.Rxc3 Rxd4 37.b4 Bc4 38.Kf2 g5 39.Re3 Be6"
                "40.Rc3 Bc4 41.Re3 Rd2+ 42.Ke1 Rd3 43.Kf2 Kg6 44.Rxd3 Bxd3"
                "45.Ke3 Bc2 46.Kd4 Kf5 47.Kd5 h5 0-1"]))
(def example-moves
  (string/join "\n"
               ["1.e4 e5 2.Nf3 Nf6 3.Nc3 Nc6 4.d4 exd4 5.Nxd4 Bb4 6.Nxc6 bxc6"
                "7.Bd3 d5 8.exd5 cxd5 9.O-O O-O 10.Bg5 c6 11.Qf3 Be7 12.Rae1"
                "Re8 13.Ne2 h6 14.Bf4 Bd6 15.Nd4 Bg4 16.Qg3 Bxf4 17.Qxf4 Qb6"
                "18.c4 Bd7 19.cxd5 cxd5 20.Rxe8+ Rxe8 21.Qd2 Ne4 22.Bxe4 dxe4"
                "23.b3 Rd8 24.Qc3 f5 25.Rd1 Be6 26.Qe3 Bf7 27.Qc3 f4 28.Rd2 Qf6"
                "29.g3 Rd5 30.a3 Kh7 31.Kg2 Qe5 32.f3 e3 33.Rd3 e2 34.gxf4 e1=Q"
                "35.fxe5 Qxc3 36.Rxc3 Rxd4 37.b4 Bc4 38.Kf2 g5 39.Re3 Be6"
                "40.Rc3 Bc4 41.Re3 Rd2+ 42.Ke1 Rd3 43.Kf2 Kg6 44.Rxd3 Bxd3"
                "45.Ke3 Bc2 46.Kd4 Kf5 47.Kd5 h5 0-1"]))

(deftest remove-metadata-test
  (let [game-without-metadata (remove-metadata example-pgn)]
    (is (not (re-find #"\[" game-without-metadata)))
    (is (not (re-find #"\]" game-without-metadata)))))

(deftest move-regex-test
  (testing "Can match valid moves"
    (is (re-find move-regex "1. Qc3 0-0"))
    (is (re-find move-regex "11.Rd2+ Ke1"))
    (is (re-find move-regex "15.Rxd2+\nKe1")))

  (testing "Can match moves when white plays last (no move for black, he lost or tie)"
    (doseq [last-move ["47.Kd5+ 0-1" "47.Nxe5 1\\2-1\\2" "47. Kd5+\n0-1"]]
      (let [match (re-find move-regex last-move)]
        (is (not (nil? match)))
        (is (not= "0-1" (last match))))))

  (testing "Can match multiple moves"
    (let [moves               "44.Rxd3 Bxd3 45.Ke3 Bc2 46.Kd4 Kf5 47.Kd5 h5 0-1"
          expected-move-count 4]
      (is (= expected-move-count (count (re-seq move-regex moves))))))

  (testing "Can extract all moves in example moves"
    (is (= 47 (count (re-seq move-regex example-moves))))))
