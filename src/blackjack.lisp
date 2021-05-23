(in-package #:blackjack)

(defparameter *52-cards-deck*
  '(1 1 1 1 2 2 2 2 3 3 3 3 4 4 4 4 5 5 5 5 6 6 6 6 7 7 7 7 8 8 8 8 9 9 9 9 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10)
  "List of cards in a deck.
For now, I will consider that the value of Aces is 1 (it could be 1 or 11, depending of the hand).")

(defclass deck ()
  ((cards :initarg :cards
          :initform nil
          :accessor cards)))

(defun make-deck (&optional (number-of-decks 1))
  "Make a deck which cards are already shuffled."
  (make-instance 'deck :cards (alexandria:shuffle
                               (alexandria:flatten (loop :repeat number-of-decks
                                                         :collect *52-cards-deck*)))))

(defmethod shuffle ((deck deck))
  "Shuffle a deck."
  (setf (cards deck) (alexandria:shuffle (cards deck)))
  deck)

(defclass player ()
  ((name :initarg :name
         :initform nil
         :accessor name)
   (hand :initarg :hand
         :initform nil
         :accessor hand)
   (points :initarg :points
           :initform 0
           :accessor points)))

(defun make-player (name &optional hand (points 0))
  (make-instance 'player :name name :hand hand :points points))

(defmethod hit ((player player) (deck deck))
  (let ((card (pop (cards deck))))
    (setf (hand player) (nconc (hand player) (list card)))
    (setf (points player) (reduce #'+ (hand player)))
    card))

(defclass game ()
  ((players :initarg :players
            :initform nil
            :accessor players)
   (deck :initarg :deck
         :initform nil
         :accessor deck)))

(defun make-game (number-of-players &optional (number-of-decks 1))
  (make-instance 'game
                 :players (loop :for x :from 1 :to number-of-players
                                :collect (make-player (format nil "player-~a" x)))
                 :deck (make-deck number-of-decks)))

(defmethod deal-first-hand ((game game))
  "Deal the first hand (2 cards to each player)."
  (let ((players (players game))
        (deck (deck game)))
    (dotimes (x 2)
      (dolist (player players)
        (hit player deck)))))

;;; ------------------------------- ;;;
;;; Example of how to play the game ;;;
;;; ------------------------------- ;;;

;; Create a 3 players, 1 deck black jack game
(defparameter game (make-game 3))

;; Deal the first hand
(deal-first-hand game)

;; Get the list of players and their slots values
(defparameter players (players game))

(mapcar #'name players)   ;=> ("player-1" "player-2" "player-3")
(mapcar #'hand players)   ;=> ((10 10) (2 3) (10 1))
(mapcar #'points players) ;=> (20 5 11)

(mapcar (lambda (player)
          (format t "~a: ~d points~%" (name player) (points player)))
        players)
;=> player-1: 20 points
;=> player-2: 5 points
;=> player-3: 11 points
