(defpackage #:advent-of-code-2023/day-7
  (:use #:cl)
  (:import-from #:serapeum
                #:econd
                #:~>
                #:lines)
  (:import-from #:uiop
                #:read-file-string)
  (:import-from #:cl-ppcre
                #:split)
  (:import-from #:alexandria
                #:hash-table-keys
                #:hash-table-values)
  (:export
   #:*example*
   #:load-input
   #:part-1
   #:part-2))

(in-package #:advent-of-code-2023/day-7)

(defparameter *example* "32T3K 765
T55J5 684
KK677 28
KTJJT 220
QQQJA 483")

(defun load-input ()
  (read-file-string "day-7.input"))

(defun parse-input-1 (input)
  (~> (lines input)
      (mapcar #'parse-line-1 _)))

(defun parse-line-1 (line)
  (destructuring-bind (hand-string bid-string) (split " " line)
    (list (parse-hand-1 hand-string)
          (parse-integer bid-string))))

(defun parse-hand-1 (hand)
  (map 'list #'parse-card-1 hand))

(defun parse-card-1 (card)
  (if (digit-char-p card)
      (- (char-code card) (char-code #\0))
      (ecase card
        (#\A 14)
        (#\K 13)
        (#\Q 12)
        (#\J 11)
        (#\T 10))))

(defun frequency-table (list &key (test 'eql))
  (loop with table = (make-hash-table :test test)
        for key in list do
          (incf (gethash key table 0))
        finally (return table)))

(defun score-hand (hand)
  (let* ((frequency-table (frequency-table hand))
         (counts (sort (hash-table-values frequency-table) #'>)))
    (list (score-signature counts) hand)))

(defun score-signature (counts)
  (econd
   ;; Five of a kind, where all five cards have the same label: AAAAA
   ((equal '(5) counts) 7)
   ;; Four of a kind, where four cards have the same label and one card has a different label: AA8AA
   ((equal '(4 1) counts) 6)
   ;; Full house, where three cards have the same label, and the remaining two cards share a different label: 23332
   ((equal '(3 2) counts) 5)
   ;; Three of a kind, where three cards have the same label, and the remaining two cards are each different from any other card in the hand: TTT98
   ((equal '(3 1 1) counts) 4)
   ;; Two pair, where two cards share one label, two other cards share a second label, and the remaining card has a third label: 23432
   ((equal '(2 2 1) counts) 3)
   ;; One pair, where two cards share one label, and the other three cards have a different label from the pair and each other: A23A4
   ((equal '(2 1 1 1) counts) 2)
   ;; High card, where all cards' labels are distinct: 23456)
   ((equal '(1 1 1 1 1) counts) 1)))

(defun list<= (xs ys)
  (loop for x in xs
        for y in ys
        for ixs = xs then (cdr ixs)
        for iys = ys then (cdr iys) do
          (cond ((< x y) (return t))
                ((> x y) (return nil)))
        finally (when ixs (return t))))

(defun hand<= (a b)
  (destructuring-bind (a-rank a-tiebreaker) (score-hand a)
    (destructuring-bind (b-rank b-tiebreaker) (score-hand b)
      (cond ((< a-rank b-rank) t)
            ((> a-rank b-rank) nil)
            (t (list<= a-tiebreaker b-tiebreaker))))))

(defun total-winnings (hands-bids cmp)
  (loop with hands = (sort (mapcar #'car hands-bids) cmp)
        with index = (loop with table = (make-hash-table :test 'equal)
                           for i = 1 then (1+ i)
                           for hand in hands
                           do (setf (gethash hand table) i)
                           finally (return table))
        for (hand bid) in hands-bids
        for rank = (gethash hand index)
        for winning = (* bid rank)
        summing winning))

(defun part-1 (input)
  (~> (parse-input-1 input)
      (total-winnings _ #'hand<=)))

(defun parse-input-2 (input)
  (~> (lines input)
      (mapcar #'parse-line-2 _)))

(defun parse-line-2 (line)
  (destructuring-bind (hand-string bid-string) (split " " line)
    (list (parse-hand-2 hand-string)
          (parse-integer bid-string))))

(defun parse-hand-2 (hand)
  (map 'list #'parse-card-2 hand))

(defun parse-card-2 (card)
  (if (digit-char-p card)
      (- (char-code card) (char-code #\0))
      (ecase card
        (#\A 14)
        (#\K 13)
        (#\Q 12)
        (#\J 1)
        (#\T 10))))

(defun joker-signature (hand)
  (let* ((table (frequency-table hand))
         (counts (print (sort (hash-table-values table) #'>)))
         (jokers (gethash 1 table 0)))
    (econd
     ((= jokers 5) '(5))
     ((= jokers 4) '(5))
     ((= jokers 3) (econd ((equal '(3 2) counts) '(5))
                          ((equal '(3 1 1) counts) '(4 1))))
     ((= jokers 2) (econd
                    ((equal '(3 2) counts) '(5))
                    ((equal '(2 2 1) counts) '(4 1))
                    ((equal '(2 1 1 1) counts) '(3 1 1))))
     ((= jokers 1) (econd
                    ((equal '(4 1) counts) '(5))
                    ((equal '(3 1 1) counts) '(4 1))
                    ((equal '(2 2 1) counts) '(3 2))
                    ((equal '(2 1 1 1) counts) '(3 1 1))
                    ((equal '(1 1 1 1 1) counts) '(2 1 1 1))))
     ((= jokers 0) counts))))

(defun score-hand-with-jokers (hand)
  (list (score-signature (joker-signature hand))
        hand))

(defun hand-with-jokers<= (a b)
  (destructuring-bind (a-rank a-tiebreaker) (score-hand-with-jokers a)
    (destructuring-bind (b-rank b-tiebreaker) (score-hand-with-jokers b)
      (cond ((< a-rank b-rank) t)
            ((> a-rank b-rank) nil)
            (t (list<= a-tiebreaker b-tiebreaker))))))

(defun part-2 (input)
  (~> (parse-input-2 input)
      (total-winnings _ #'hand-with-jokers<=)))
