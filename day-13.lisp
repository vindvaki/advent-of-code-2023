(defpackage #:advent-of-code-2023/day-13
  (:use #:cl)
  (:import-from #:serapeum
                #:~>
                #:lines)
  (:import-from #:uiop
                #:read-file-string)
  (:import-from #:cl-ppcre
                #:split)
  (:import-from #:alexandria
                #:when-let
                #:eswitch))

(in-package #:advent-of-code-2023/day-13)

(defparameter *example* "#.##..##.
..#.##.#.
##......#
##......#
..#.##.#.
..##..##.
#.#.##.#.

#...##..#
#....#..#
..##..###
#####.##.
#####.##.
..##..###
#....#..#")

(defun print-grid (grid &optional (out *standard-output*))
  (loop with (rows cols) = (array-dimensions grid)
        for row from 0 upto (1- rows) do
          (loop for col from 0 upto (1- cols)
                do (write-char (aref grid row col) out)
                finally (write-char #\Newline out))
        finally (write-char #\Newline)))

(defun parse-input (input)
  (~> (split "\\n\\n" input)
      (mapcar #'parse-grid _)))

(defun parse-grid (input)
  (let* ((lines (lines input))
         (rows (length lines))
         (cols (length (car lines)))
         (line-lists (mapcar (lambda (x) (coerce x 'list)) lines)))
    (make-array (list rows cols) :initial-contents line-lists)))

(defun find-mirror-column (grid &optional (error-count 0))
  (loop with (rows cols) = (array-dimensions grid)
        for col from 0 upto (- cols 2)
        when (= error-count
                (loop for lhs from col downto 0
                      for rhs from (1+ col) upto (1- cols)
                      summing (loop for row from 0 upto (1- rows)
                                    counting (char/= (aref grid row lhs)
                                                     (aref grid row rhs)))))
          return col))

(defun find-mirror-row (grid &optional (error-count 0))
  (loop with (rows cols) = (array-dimensions grid)
        for row from 0 upto (- rows 2)
        when (= error-count
                (loop for upper from row downto 0
                      for lower from (1+ row) upto (1- rows)
                      summing (loop for col from 0 upto (1- cols)
                                    counting (char/= (aref grid upper col)
                                                     (aref grid lower col)))))
          return row))

(defun load-input ()
  (read-file-string "day-13.input"))

(defun part-1 (input)
  (loop for grid in (parse-input input)
        for row = (find-mirror-row grid)
        for col = (find-mirror-column grid)
        when row
          summing (* 100 (1+ row))
        when col
          summing (1+ col)
        when (not (or row col))
          do (print-grid grid)))

(defun opposite (char)
  (eswitch (char)
    (#\. #\#)
    (#\# #\.)))

(defun part-2 (input)
  (loop for grid in (parse-input input)
        for row = (find-mirror-row grid 1)
        for col = (find-mirror-column grid 1)
        when row
          summing (* 100 (1+ row))
        when col
          summing (1+ col)
        when (not (or row col))
          do (print-grid grid)))
