(in-package #:blackjack)

;; Create a 3 players, 1 deck black jack game
(defparameter *game* (make-game 3))

;; Deal the first hand
(deal-first-hand *game*)

;; Print the points of the dealer and of each player after the first hand
(let ((players (players *game*))
      (dealer (dealer *game*)))
  (format t "~a: ~d points~%" (name dealer) (points dealer))
  (mapcar (lambda (player)
            (format t "~a: ~d points~%" (name player) (points player)))
          players))
;=> dealer: 11 points
;=> player-1: 20 points
;=> player-2: 5 points
;=> player-3: 11 points
