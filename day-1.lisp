(defpackage #:advent-of-code-2023/day-1
  (:use #:cl)
  (:import-from #:serapeum
                #:lines))

(in-package #:advent-of-code-2023/day-1)

(defparameter *example* "1abc2
pqr3stu8vwx
a1b2c3d4e5f
treb7uchet")

(defun part-1 (input)
  (loop for line in (lines input)
        summing (parse-integer (delete-if-not #'digit-char-p line))))
