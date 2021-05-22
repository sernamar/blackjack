(defpackage #:blackjack
  (:use #:cl)
  (:export #:get-points
           #:hit
           #:create-deck
           #:create-player
           #:hand
           #:*52-cards-deck*
           #:cards))
