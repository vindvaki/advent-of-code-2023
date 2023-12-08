(defpackage #:advent-of-code-2023/day-7
  (:use #:cl)
  (:import-from #:serapeum
                #:~>
                #:lines)
  (:import-from #:uiop
                #:read-file-string)
  (:import-from #:cl-ppcre
                #:split)
  (:import-from #:alexandria
                #:eswitch
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

(defun parse-input (input parse-card-fn)
  (loop for line in (lines input)
        for (hand-string bid-string) = (split " " line)
        collecting (list (map 'list parse-card-fn hand-string)
                         (parse-integer bid-string))))

(defun parse-card (card)
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

(defun hand-signature (hand)
  (let* ((frequency-table (frequency-table hand)))
    (sort (hash-table-values frequency-table) #'>)))

(defun deep-list< (xs ys)
  "Deep compare two number-leafed lists under a recursive lexicographical ordering"
  (assert (equal (listp xs) (listp ys)))
  (unless (listp xs)
    (return-from deep-list< (< xs ys)))
  (loop for x in xs
        for y in ys
        for ixs = xs then (cdr ixs)
        for iys = ys then (cdr iys) do
          (cond ((deep-list< x y) (return t))
                ((deep-list< y x) (return nil)))
        finally (when (and (not ixs) iys)
                  (return t))))

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

(defun make-hand< (signature-function)
  (lambda (a b) (deep-list< (list (funcall signature-function a) a)
                            (list (funcall signature-function b) b))))

(defun part-1 (input)
  (~> (parse-input input #'parse-card)
      (total-winnings _ (make-hand< #'hand-signature))))

(defun parse-joker-card (card)
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
         (counts (sort (hash-table-values table) #'>))
         (jokers (gethash 1 table 0)))
    (ecase jokers
      (5 '(5))
      (4 '(5))
      (3 (eswitch (counts :test 'equal)
           ('(3 2) '(5))
           ('(3 1 1) '(4 1))))
      (2 (eswitch (counts :test 'equal)
           ('(3 2) '(5))
           ('(2 2 1) '(4 1))
           ('(2 1 1 1) '(3 1 1))))
      (1 (eswitch (counts :test 'equal)
           ('(4 1) '(5))
           ('(3 1 1) '(4 1))
           ('(2 2 1) '(3 2))
           ('(2 1 1 1) '(3 1 1))
           ('(1 1 1 1 1) '(2 1 1 1))))
      (0 counts))))

(defun part-2 (input)
  (~> (parse-input input #'parse-joker-card)
      (total-winnings _ (make-hand< #'joker-signature))))
