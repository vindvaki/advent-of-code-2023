(defpackage #:advent-of-code-2023/day-9
  (:use #:cl)
  (:import-from #:serapeum
                #:~>
                #:lines)
  (:import-from #:uiop
                #:read-file-string)
  (:import-from #:cl-ppcre
                #:split)
  (:import-from #:alexandria
                #:last-elt)
  (:export
   #:part-1
   #:part-2
   #:*example*
   #:load-input))

(in-package #:advent-of-code-2023/day-9)

(defparameter *example* "0 3 6 9 12 15
1 3 6 10 15 21
10 13 16 21 30 45")

(defun load-input ()
  (read-file-string "day-9.input"))

(defun parse-input (input)
  (~> (lines input)
      (mapcar #'parse-line _)))

(defun parse-line (line)
  (~> (split " " line)
      (mapcar #'parse-integer _)))

(defun sequential-differences (numbers)
  (loop for (a b) on numbers while b collecting (- b a)))

(defun difference-tower (numbers)
  (loop for sequence = numbers then differences
        for differences = (sequential-differences sequence)
        collect sequence
        while (cdr (remove-duplicates sequence))))

(defun extrapolate-history (numbers)
  (let ((tower (difference-tower numbers)))
    (loop for differences in (reverse tower)
          for result = (last-elt differences) then (+ result (last-elt differences))
          finally (return result))))

(defun part-1 (input)
  (loop for numbers in (parse-input input)
        summing (extrapolate-history numbers)))

(defun part-2 (input)
  (loop for numbers in (parse-input input)
        summing (extrapolate-history (reverse numbers))))
