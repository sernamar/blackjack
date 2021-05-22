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
  ((hand :initarg :hand
         :initform nil
         :accessor hand)))

(defun make-player (&optional hand)
  (make-instance 'player :hand hand))

(defmethod hit ((player player) (deck deck))
  (let ((card (pop (cards deck))))
    (setf (hand player) (nconc (hand player) (list card)))
    card))

(defmethod get-points ((player player))
  (reduce #'+ (hand player)))

(defclass game ()
  ((players :initarg :players
            :initform nil
            :accessor players)
   (deck :initarg :deck
         :initform nil
         :accessor deck)))

(defun make-game (number-of-players &optional (number-of-decks 1))
  (make-instance 'game
                 :players (loop :repeat number-of-players
                                :collect (make-player))
                 :deck (make-deck number-of-decks)))

(defmethod deal-first-2-cards ((game game))
  "Deal 2 cards to each player."
  (let ((players (players game))
        (deck (deck game)))
    (format t "Cards in the deck: ~a~%" (cards deck))
    (dotimes (x 2)
      (dolist (player players)
        (hit player deck)))
    ;; print players hand
    (dolist (player players)
      (format t "~a~%" (hand player)))))
