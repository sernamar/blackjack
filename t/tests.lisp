(defpackage #:blackjack-tests
  (:use #:cl
        #:blackjack)
  (:export #:all-tests
           #:run-tests))

(in-package #:blackjack-tests)

(fiveam:def-suite all-tests
  :description "Test suite to run all tests")

(fiveam:in-suite all-tests)

(defun run-tests ()
  (fiveam:run! 'all-tests))

;; Tests for the DECK class ;;

(fiveam:test test-deck
  (let ((number-of-cards 52)
        (1-deck (make-deck))
        (6-deck (make-deck 6)))
    (fiveam:is (= number-of-cards (length (cards 1-deck))))
    (fiveam:is (= (* 6 number-of-cards) (length (cards 6-deck))))))

(fiveam:test test-shuffle
  (let* ((deck (make-deck))
         (shuffled-deck (shuffle deck)))
    (fiveam:is (= (length (cards deck))
                  (length (cards shuffled-deck))))))

;; Tests for the PLAYER class ;;

(fiveam:test test-player
  (let* ((name "Mike")
         (empty-hand nil)
         (player-1 (make-player name))
         (hand '(1 2 3))
         (points 6)
         (player-2 (make-player name hand points)))
    (fiveam:is (equal name
                      (name player-1)))
    (fiveam:is (equal empty-hand
                      (hand player-1)))
    (fiveam:is (= 0
                  (points player-1)))
    (fiveam:is (equal name
                      (name player-2)))
    (fiveam:is (equal hand
                      (hand player-2)))
    (fiveam:is (= points
                  (points player-2)))))

(fiveam:test test-hit
  (let ((player (make-player "Mike"))
        (deck (make-deck))
        (cards '(1 3 5 7))
        (expected 1)
        (expected-hand '(1)))
    (setf (cards deck) cards) ; set the cards slot of deck for tests purposes
    (fiveam:is (= expected (hit player deck)))
    (fiveam:is (equal expected-hand (hand player)))))

(fiveam:test test-points-after-hits
  (let ((player (make-player "Mike"))
        (deck (make-deck))
        (cards '(1 5 7))
        (expected-after-1-hit 11)
        (expected-after-2-hits 16)
        (expected-after-3-hits 13))
    (setf (cards deck) cards) ; set the cards slot of deck for tests purposes
    (hit player deck)
    (fiveam:is (= expected-after-1-hit (points player)))
    (hit player deck)
    (fiveam:is (= expected-after-2-hits (points player)))
    (hit player deck)
    (fiveam:is (= expected-after-3-hits (points player)))))

;; Tests for the GAME class ;;

(fiveam:test test-game
  (let* ((number-of-players 3)
         (number-of-cards-per-deck 52)
         (number-of-decks 1)
         (game (make-game number-of-players number-of-decks)))
    (fiveam:is (= number-of-players
                  (length (players game))))
    (fiveam:is (= (* number-of-cards-per-deck number-of-decks)
                  (length (cards (deck game)))))))
