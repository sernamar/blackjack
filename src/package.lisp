(defpackage #:blackjack
  (:use #:cl)
  (:export ;; deck class
           #:make-deck
           #:cards
           #:shuffle
           ;; player class
           #:make-player
           #:hand
           #:hit
           #:get-points))
