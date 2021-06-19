(in-package #:blackjack)

;; Create a 3 players, 1 deck black jack game
(defparameter *game* (make-game 3))

;; Deal the first hand
(deal-first-hand *game*)

;; Deal next hands
(deal-next-hands *game*)
