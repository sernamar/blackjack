(in-package #:blackjack)

(defun get-points (hand)
  (reduce #'+ hand))
