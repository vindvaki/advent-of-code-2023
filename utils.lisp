(defpackage #:advent-of-code-2023/utils
  (:use #:cl)
  (:import-from #:serapeum
                #:lines)
  (:export
   #:print-grid
   #:parse-grid))

(in-package #:advent-of-code-2023/utils)

(defun print-grid (grid &optional (out *standard-output*))
  (loop with (rows cols) = (array-dimensions grid)
        for row from 0 upto (1- rows) do
          (loop for col from 0 upto (1- cols)
                do (write-char (aref grid row col) out)
                finally (write-char #\Newline out))
        finally (write-char #\Newline)))

(defun parse-grid (input)
  (let* ((lines (lines input))
         (rows (length lines))
         (cols (length (car lines)))
         (line-lists (mapcar (lambda (x) (coerce x 'list)) lines)))
    (make-array (list rows cols) :initial-contents line-lists)))
