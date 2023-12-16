(defpackage #:advent-of-code-2023/utils
  (:use #:cl)
  (:import-from #:serapeum
                #:lines)
  (:import-from #:alexandria
                #:with-gensyms)
  (:export
   #:print-grid
   #:parse-grid
   #:reducing))

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

(defmacro reducing ((function yield &optional (initial ''%none)) &body body)
  "Exposes a function `yield' within `body' and returns the result of reducing
`function' over the arguments given to `yield'. Returns `nil' if the function
named by `yield' is never called and no `initial' value is provided.

This is a generalization of Serapeum's `summing' and `collecting'. A few
examples:

   (reducing (#'+ sum) (dotimes (i 10) (sum i)))
   (reducing (#'* mul) (dotimes (i 10) (mul i)))
   (reducing (#'min minimize) (dotimes (i 10) (minimize i)))
   (reducing (#'max maximize) (dotimes (i 10) (maximize i)))

"
  (with-gensyms (result arg called function-value)
    `(let ((,result ,initial)
           (,function-value ,function)
           (,called (not (eq ,initial '%none))))
       (labels ((,yield (,arg)
                  (if ,called
                      (setf ,result (funcall ,function-value ,result ,arg))
                      (progn (setf ,result ,arg)
                             (setf ,called t)))))
         ,@body)
       (if ,called
           ,result
           nil))))
