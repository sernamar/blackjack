(in-package #:blackjack)

(defparameter *cards*
  '(1 1 1 1 2 2 2 2 3 3 3 3 4 4 4 4 5 5 5 5 6 6 6 6 7 7 7 7 8 8 8 8 9 9 9 9 10 10 10 10)
  "List of cards in a deck.
For now, I will consider that the value of Aces is 1 (it could be 1 or 11, depending of the hand).")

(defun create-deck (&optional (number-of-decks 1))
  "Create a deck which cards are already shuffled."
  (alexandria:flatten (loop :repeat number-of-decks
                            :collect (shuffle *cards*))))
(defun shuffle (deck)
  "Shuffle a deck."
  (alexandria:shuffle deck))

(defun get-points (hand)
  (reduce #'+ hand))

(defun remove-nth (n list)
  (declare
   (type (integer 0) n)
   (type list list))
  (if (or (zerop n) (null list))
      (cdr list)
      (cons (car list) (remove-nth (1- n) (cdr list)))))

(defun hit (deck)
  "Get a card from the deck.
It returns the card and a new deck without that card."
  (let* ((position (random (length deck)))
         (card (nth position deck)))
    (values card
            (remove-nth position deck))))
