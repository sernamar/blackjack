(in-package #:blackjack)

;; Create a 3 players, 1 deck black jack game
(defparameter *game* (make-game 3))

;; Deal the first hand
(deal-first-hand *game*)

;; Get the list of players and their slots values
(defparameter *players* (players *game*))

(mapcar #'name *players*)   ;=> ("player-1" "player-2" "player-3")
(mapcar #'hand *players*)   ;=> ((10 10) (2 3) (10 1))
(mapcar #'points *players*) ;=> (20 5 11)

;; Get the dealer and his slots values
(defparameter *dealer* (dealer *game*))

(name *dealer*)
(hand *dealer*)
(points *dealer*)

;; Print the point of each player after the first hand
(mapcar (lambda (player)
          (format t "~a: ~d points~%" (name player) (points player)))
        *players*)
(format t "~a: ~d points~%" (name *dealer*) (points *dealer*))
;=> player-1: 20 points
;=> player-2: 5 points
;=> player-3: 11 points
;=> dealer: 11 points
