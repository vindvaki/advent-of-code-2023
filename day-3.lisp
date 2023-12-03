(defpackage #:advent-of-code-2023/day-3
  (:use #:cl)
  (:import-from #:serapeum
                #:~>
                #:lines)
  (:import-from #:uiop
                #:read-file-string))

(in-package #:advent-of-code-2023/day-3)

(defparameter *example* "467..114..
...*......
..35..633.
......#...
617*......
.....+.58.
..592.....
......755.
...$.*....
.664.598..")

(defstruct (number-interval (:constructor make-number-interval (number row col-begin col-end)))  number row col-begin col-end)

(defun parse-input (input)
  (loop with lines = (lines input)
        with rows = (length lines)
        with cols = (length (car lines))
        with char-grid = (make-array (list rows cols) :initial-contents (mapcar (lambda (s) (coerce s 'list)) lines))
        with number-intervals = nil
        for row from 0 to (1- rows) do
          (loop with digit-begin = nil
                with number = 0
                for col from 0 to (1- cols)
                for char = (aref char-grid row col)
                do (if (digit-char-p char)
                       (progn
                         (setf digit-begin (or digit-begin col)
                               number (+ (* 10 number) (- (char-code char) (char-code #\0)))))
                       (when digit-begin
                         (push (make-number-interval number row digit-begin col) number-intervals)
                         (setf digit-begin nil
                               number 0)))
                finally (when digit-begin
                          (push (make-number-interval number row digit-begin col) number-intervals)))
        finally (return (values char-grid number-intervals))))

(defun symbol-adjacent-p (grid interval)
  (loop with (rows cols) = (array-dimensions grid)
        with irow = (number-interval-row interval)
        with icols-begin = (number-interval-col-begin interval)
        with icols-end = (number-interval-col-end interval)
        for row from (1- irow) upto (1+ irow)
        when (and (<= 0 row (1- rows))
                  (loop for col from (1- icols-begin) upto icols-end
                        when (and (<= 0 col (1- cols)) ;; in the grid
                                  (not (and (= row irow) (<= icols-begin col (1- icols-end)))) ;; not in the current interval
                                  (char/= #\. (aref grid row col))) ;; is a symbol
                          do (return t)))
          do (return t)))

(defun load-input ()
  (read-file-string "day-3.input"))

(defun part-1 (input)
  (multiple-value-bind (grid intervals) (parse-input input)
    (loop for interval in intervals
          when (symbol-adjacent-p grid interval)
            summing (number-interval-number interval))))

(defun interval-neighbor-p (interval row col)
  (let ((irow (number-interval-row interval))
        (icol-begin (number-interval-col-begin interval))
        (icol-end (number-interval-col-end interval)))
    (and (<= (1- irow) row (1+ irow))
         (<= (1- icol-begin) col icol-end))))

(defun part-2 (input)
  (multiple-value-bind (grid intervals) (parse-input input)
    (loop with (rows cols) = (array-dimensions grid)
          with result = 0
          for row from 0 to (1- rows) do
            (loop for col from 0 to (1- cols) do
              (when (char= #\* (aref grid row col))
                (let ((neighbors (loop for interval in intervals
                                       when (interval-neighbor-p interval row col)
                                         collect (number-interval-number interval))))
                  (when (= 2 (length neighbors))
                    (incf result (reduce #'* neighbors))))))
          finally (return result))))
