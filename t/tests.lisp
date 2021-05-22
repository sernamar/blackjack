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
        (1-deck (create-deck))
        (6-deck (create-deck 6)))
    (fiveam:is (= number-of-cards (length (cards 1-deck))))
    (fiveam:is (= (* 6 number-of-cards) (length (cards 6-deck))))))

;; Tests for the PLAYER class ;;

(fiveam:test test-player
  (let* ((empty-hand nil)
         (player-1 (create-player))
         (hand '(1 2 3))
         (player-2 (create-player :hand hand)))
    (fiveam:is (equal empty-hand
                  (hand player-1)))
    (fiveam:is (equal hand
                  (hand player-2)))))

(fiveam:test test-get-points
  (let ((player (create-player :hand '(2 9)))
        (expected 11))    
    (fiveam:is (= expected (get-points player)))))

(fiveam:test test-hit
  (let ((player (create-player))
        (deck '(1 3 5 7 9))
        (expected 1)
        (expected-hand '(1)))
    (fiveam:is (= expected (hit player deck)))
    (fiveam:is (equal expected-hand (hand player)))))

