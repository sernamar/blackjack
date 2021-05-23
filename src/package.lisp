(defpackage #:blackjack
  (:use #:cl)
  (:export ;; deck class
           #:make-deck
           #:cards
           #:shuffle
           ;; player class
           #:make-player
           #:name
           #:hand
           #:points
           #:hit
           #:get-points
           ;; game class
           #:make-game
           #:players
           #:deck))
