(in-package #:blackjack)

;;; -------- ;;;
;;;   Deck   ;;;
;;; -------- ;;;

(defparameter *52-cards-deck*
  '(11 11 11 11 2 2 2 2 3 3 3 3 4 4 4 4 5 5 5 5 6 6 6 6 7 7 7 7 8 8 8 8 9 9 9 9 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10)
  "List of cards in a deck.")

(defclass deck ()
  ((cards :initarg :cards
          :initform nil
          :accessor cards)))

(defun make-deck (&optional (number-of-decks 1))
  "Make a deck which cards are already shuffled."
  (make-instance 'deck :cards (alexandria:shuffle
                               (alexandria:flatten (loop :repeat number-of-decks
                                                         :collect *52-cards-deck*)))))

(defmethod shuffle ((deck deck))
  "Shuffle a deck."
  (setf (cards deck) (alexandria:shuffle (cards deck)))
  deck)

;;; ---------- ;;;
;;;   Player   ;;;
;;; ---------- ;;;

(defclass player ()
  ((name :initarg :name
         :initform nil
         :accessor name)
   (hand :initarg :hand
         :initform nil
         :accessor hand)
   (points :initarg :points
           :initform 0
           :accessor points)))

(defun make-player (name &optional hand (points 0))
  (make-instance 'player :name name :hand hand :points points))

(defmethod hit ((player player) (deck deck))
  (let ((card (pop (cards deck))))
    (setf (hand player) (nconc (hand player) (list card)))
    (setf (points player) (choose-best-score (hand player)))
    card))

(defmethod stand ((player player) dealer-face-up-card)
  "Decide if a player should stand or hit again.

For now, the only strategies are:
  - Stand on anything 17 or higher.
  - If the dealer’s card is a four, five or six, do not bust. It is common practice to hit on anything less than 12, and stand otherwise."
  (let ((points (points player)))
    (cond ((<= 17 points) t)
          ((and (member dealer-face-up-card '(4 5 6)) (<= 12 points)) t)
          (t nil))))

(defun choose-best-score (hand)
  "If it is an Ace in the hand, choose its value (1 or 11) to get the best score (closest to 21)."
  (let ((sum (reduce #'+ hand)))
    (if (and (member 11 hand) (> sum 21))
        (- sum 10)
        sum)))

(defmethod bust ((player player))
  (< 21 (points player)))

(defun print-players-scores (players)
  (mapcar (lambda (player)
            (if (< 21 (points player))
                (format t "~a did bust!~%" (name player))
                (format t "~a: ~d points~%" (name player) (points player))))
            players))

;;; -------- ;;;
;;;   Game   ;;;
;;; -------- ;;;

(defclass game ()
  ((players :initarg :players
            :initform nil
            :accessor players)
   (dealer :initarg :dealer
           :initform nil
           :accessor dealer)
   (deck :initarg :deck
         :initform nil
         :accessor deck)))

(defun make-game (number-of-players &optional (number-of-decks 1))
  (make-instance 'game
                 :players (loop :for x :from 1 :to number-of-players
                                :collect (make-player (format nil "player-~a" x)))
                 :dealer (make-player "dealer")
                 :deck (make-deck number-of-decks)))

(defmethod deal-first-hand ((game game))
  "Deal the first hand (2 cards to each player)."
  (let ((players (players game))
        (dealer (dealer game))
        (deck (deck game)))
    (dotimes (x 2)
      (dolist (player players)
        (hit player deck))
      (hit dealer deck))))

(defmethod deal-next-hands ((game game))
  (let* ((players (players game))
         (dealer (dealer game))
         (dealer-face-up-card (first (hand dealer)))
         (deck (deck game)))
    (dolist (player players)
      (loop :while (and (not (bust player)) (not (stand player dealer-face-up-card)))
            :do (progn
                  (hit player deck)
                  (format t "~a asks for a card, and now he has: ~d points~%" (name player) (points player)))))))

;;; ----------- ;;;
;;;   Example   ;;;
;;; ----------- ;;;

(defun run-example ()
  (let* ((number-of-players 3)
         (number-of-decks 6)
         (game (make-game number-of-players number-of-decks))
         (players (players game))
         (dealer (dealer game)))
    (format t "Starting the game. Good luck everyone!~%")

    (deal-first-hand game)
    (format t "~%Scores after dealing the first hand:~%")
    (print-players-scores players)

    (format t "~%Dealer's face-up card: ~a~%" (first (hand dealer)))
    
    (deal-next-hands game)
    (format t "~%Final scores:~%")
    (format t "~%Dealer's score: ~a~%" (points dealer))
    (print-players-scores players)))
