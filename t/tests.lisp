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
    (fiveam:is (member (hit deck) deck))))
