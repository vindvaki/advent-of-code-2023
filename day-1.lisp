(defpackage #:advent-of-code-2023/day-1
  (:use #:cl)
  (:import-from #:serapeum
                #:defconst
                #:lines)
  (:import-from #:uiop
                #:read-file-string)
  (:import-from #:alexandria
                #:when-let
                #:first-elt
                #:last-elt))

(in-package #:advent-of-code-2023/day-1)

(defparameter *example* "1abc2
pqr3stu8vwx
a1b2c3d4e5f
treb7uchet")

(defparameter *example-2* "two1nine
eightwothree
abcone2threexyz
xtwone3four
4nineeightseven2
zoneight234
7pqrstsixteen")

(defun load-input ()
  (read-file-string "day-1.input"))

(defun part-1 (input)
  (loop for line in (lines input)
        for digits = (delete-if-not #'digit-char-p line)
        for first = (first-elt digits)
        for last = (last-elt digits)
        for string = (coerce (list first last) 'string)
        summing (parse-integer string)))

(defconst +digit-strings+
  '("1" "2" "3" "4" "5" "6" "7" "8" "9"
    "one" "two" "three" "four" "five" "six" "seven" "eight" "nine"))

(defun part-2 (input)
  (loop for line in (lines input)
        for first = (find-digit line)
        for last = (find-digit line :from-end t)
        for number = (+ (* 10 first) last)
        summing number))

(defun find-digit (seq &key (from-end nil))
  (loop with seq-idx = nil
        with result-idx = nil
        with cmp = (if from-end #'> #'<)
        for elt in +digit-strings+
        for elt-idx = 0 then (1+ elt-idx)
        for found-idx = (search elt seq :from-end from-end :test #'string=)
        when (and found-idx
                  (or (not seq-idx) (funcall cmp found-idx seq-idx)))
          do (setf seq-idx found-idx
                   result-idx elt-idx)
        finally (return (1+ (if (< result-idx 9)
                                result-idx
                                (- result-idx 9))))))
