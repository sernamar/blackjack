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
  (let* ((empty-hand nil)
         (player-1 (make-player))
         (hand '(1 2 3))
         (player-2 (make-player hand)))
    (fiveam:is (equal empty-hand
                      (hand player-1)))
    (fiveam:is (equal hand
                      (hand player-2)))))

(fiveam:test test-get-points
  (let ((player (make-player '(2 9)))
        (expected 11))    
    (fiveam:is (= expected (get-points player)))))

(fiveam:test test-hit
  (let ((player (make-player))
        (deck (make-deck))
        (expected 1)
        (expected-hand '(1)))
    (setf (cards deck) '(1 3 5 7)) ; set the cards slot of deck for tests purposes
    (fiveam:is (= expected (hit player deck)))
    (fiveam:is (equal expected-hand (hand player)))))

;; Tests for the GAME class ;;

(fiveam:test test-game
  (let* ((number-of-players 3)
         (number-of-decks 1)
         (game (make-game number-of-players number-of-decks)))
    (fiveam:is (= number-of-players
                  (length (players game))))
    (fiveam:is (= (* 52 number-of-decks)
                  (length (cards (deck game)))))))
