(defpackage #:blackjack
  (:use #:cl)
  (:export ;; deck class
           #:create-deck
           #:cards
           #:shuffle
           ;; player class
           #:create-player
           #:hand
           #:hit
           #:get-points))
