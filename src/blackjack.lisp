(in-package #:blackjack)

;;; -------- ;;;
;;;   Deck   ;;;
;;; -------- ;;;

(defparameter *52-cards-deck*
  '(11 11 11 11 2 2 2 2 3 3 3 3 4 4 4 4 5 5 5 5 6 6 6 6 7 7 7 7 8 8 8 8 9 9 9 9 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10)
  "List of cards in a deck.")

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

;;; ---------- ;;;
;;;   Player   ;;;
;;; ---------- ;;;

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
    (setf (points player) (choose-best-score (hand player)))
    card))

(defun choose-best-score (hand)
  "If it is an Ace in the hand, choose its value (1 or 11) to get the best score (closest to 21)."
  (let ((sum (reduce #'+ hand)))
    (if (and (member 11 hand) (> sum 21))
        (- sum 10)
        sum)))

;;; -------- ;;;
;;;   Game   ;;;
;;; -------- ;;;

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
