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

(fiveam:test test-player
  (let* ((empty-hand nil)
         (player-1 (create-player))
         (hand '(1 2 3))
         (player-2 (create-player :hand hand)))
    (fiveam:is (equal empty-hand
                  (hand player-1)))
    (fiveam:is (equal hand
                  (hand player-2)))))
