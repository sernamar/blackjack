(defpackage #:blackjack
  (:use #:cl)
  (:export ;; deck class
           #:cards
           #:make-deck
           #:shuffle
           ;; player class
           #:name
           #:hand
           #:points
           #:make-player
           #:hit
           ;; game class
           #:players
           #:deck
           #:make-game
           #:deal-first-hand))
