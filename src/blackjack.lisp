(in-package #:blackjack)

(defparameter *52-cards-deck*
  '(1 1 1 1 2 2 2 2 3 3 3 3 4 4 4 4 5 5 5 5 6 6 6 6 7 7 7 7 8 8 8 8 9 9 9 9 10 10 10 10)
  "List of cards in a deck.
For now, I will consider that the value of Aces is 1 (it could be 1 or 11, depending of the hand).")

(defun create-deck (&optional (number-of-decks 1))
  "Create a deck which cards are already shuffled."
  (shuffle (alexandria:flatten (loop :repeat number-of-decks
                                     :collect *52-cards-deck*))))

(defun shuffle (deck)
  "Shuffle a deck."
  (alexandria:shuffle deck))

(defclass player ()
  ((hand :initarg :hand
         :initform nil
         :accessor hand)))

(defun create-player (&key hand)
  (make-instance 'player :hand hand))

(defmethod hit ((player player) deck)
  (let ((card (pop deck)))
    (setf (hand player) (nconc (hand player) (list card)))
    card))

(defmethod get-points ((player player))
  (reduce #'+ (hand player)))
