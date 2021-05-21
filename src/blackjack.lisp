(in-package #:blackjack)

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
