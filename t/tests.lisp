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
  (let ((hand '(2 9))
        (points 11))    
    (fiveam:is (= points (get-points hand)))))

(fiveam:test test-hit
  (let ((deck '(1 3 5 7 9)))
    (fiveam:is (= 1 (hit deck)))))

(fiveam:test test-player
  (let* ((empty-hand nil)
         (player-1 (create-player))
         (hand '(1 2 3))
         (player-2 (create-player :hand hand)))
    (fiveam:is (equal empty-hand
                  (hand player-1)))
    (fiveam:is (equal hand
                  (hand player-2)))))
