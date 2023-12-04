(defpackage #:advent-of-code-2023/day-4
  (:use #:cl)
  (:import-from #:serapeum
                #:trim-whitespace
                #:summing
                #:~>
                #:lines)
  (:import-from #:uiop
                #:read-file-string)
  (:import-from #:cl-ppcre
                #:register-groups-bind
                #:split)
  (:export
   #:*example*
   #:part-1
   #:part-2
   #:load-input))

(in-package #:advent-of-code-2023/day-4)

(defparameter *example* "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11")

(defstruct card id winning drawn)

(defun parse-input (input)
  (mapcar #'parse-card (lines input)))

(defun parse-card (string)
  (register-groups-bind ((#'parse-integer card-id)
                         (#'parse-space-separated-numbers winning)
                         (#'parse-space-separated-numbers drawn))
      ("Card +(\\d+): *(.*) *\\| *(.*)" string)
    (make-card :id card-id :winning winning :drawn drawn)))

(defun parse-space-separated-numbers (string)
  (~> (trim-whitespace string)
      (split " +" _)
      (mapcar #'parse-integer _)))

(defun load-input ()
  (read-file-string "day-4.input"))

(defun matching-cards (card)
  (intersection (card-drawn card) (card-winning card)))

(defun part-1 (input)
  (summing
    (let ((cards (parse-input input)))
      (dolist (card cards)
        (sum (ash 1 (1- (length (matching-cards card)))))))))

(defun part-2 (input)
  (loop with cards = (parse-input input)
        with counts = (make-array (length cards) :initial-element 1)
        for card in cards
        for i = 0 then (1+ i)
        do (loop with matching = (matching-cards card)
                 with repeats = (aref counts i)
                 with count = (length matching)
                 for j from (1+ i) upto (+ i count) do
                   (incf (aref counts j) repeats))
        finally (return (reduce #'+ counts))))
