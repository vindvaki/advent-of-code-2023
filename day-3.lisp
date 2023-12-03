(defpackage #:advent-of-code-2023/day-3
  (:use #:cl)
  (:import-from #:serapeum
                #:summing
                #:collecting
                #:~>
                #:lines)
  (:import-from #:uiop
                #:read-file-string)
  (:export
   #:part-2
   #:part-1
   #:load-input
   #:*example*))

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

(defstruct (interval (:constructor make-interval (number row col-begin col-end)))
  number row col-begin col-end)

(defun parse-input (input)
  (let* ((lines (lines input))
         (rows (length lines))
         (cols (length (car lines)))
         (grid (make-array (list rows cols) :initial-contents (mapcar (lambda (s) (coerce s 'list)) lines)))
         (intervals (make-intervals grid)))
    (values grid intervals)))

(defun make-intervals (grid)
  (destructuring-bind (rows cols) (array-dimensions grid)
    (collecting
      (dotimes (row rows)
        (let ((digit-begin nil)
              (number 0))
          (dotimes (col cols)
            (let ((char (aref grid row col)))
              (if (digit-char-p char)
                  ;; then increment
                  (setf digit-begin (or digit-begin col)
                        number (+ (* 10 number) (- (char-code char) (char-code #\0))))
                  ;; else collect and reset
                  (when digit-begin
                    (collect (make-interval number row digit-begin col))
                    (setf digit-begin nil
                          number 0)))))
          ;; end-of-row needs special handling
          (when digit-begin
            (collect (make-interval number row digit-begin cols))))))))

(defun interval-contains-p (interval row col)
  (and (= row (interval-row interval))
       (<= (interval-col-begin interval) col (1- (interval-col-end interval)))))

(defun symbol-adjacent-p (grid interval)
  (destructuring-bind (rows cols) (array-dimensions grid)
    (let ((irow (interval-row interval))
          (icol-begin (interval-col-begin interval))
          (icol-end (interval-col-end interval)))
      (loop :for row :from (max (1- irow) 0) :upto (min (1+ irow) (1- rows)) :do
        (loop :for col :from (max (1- icol-begin) 0) :upto (min icol-end (1- cols)) :do
          (when (and (not (interval-contains-p interval row col))
                     (char/= #\. (aref grid row col))) ;; is a symbol
            (return-from symbol-adjacent-p t)))))))

(defun load-input ()
  (read-file-string "day-3.input"))

(defun part-1 (input)
  (multiple-value-bind (grid intervals) (parse-input input)
    (summing
      (dolist (interval intervals)
        (when (symbol-adjacent-p grid interval)
          (sum (interval-number interval)))))))

(defun interval-neighbor-p (interval row col)
  (let ((irow (interval-row interval))
        (icol-begin (interval-col-begin interval))
        (icol-end (interval-col-end interval)))
    (and (<= (1- irow) row (1+ irow))
         (<= (1- icol-begin) col icol-end))))

(defun part-2 (input)
  (multiple-value-bind (grid intervals) (parse-input input)
    (destructuring-bind  (rows cols) (array-dimensions grid)
      (summing
        (dotimes (row rows)
          (dotimes (col cols)
            (when (char= #\* (aref grid row col))
              (let ((neighbors (collecting
                                 (dolist (interval intervals)
                                   (when (interval-neighbor-p interval row col)
                                     (collect (interval-number interval)))))))
                (when (= 2 (length neighbors))
                  (sum (reduce #'* neighbors)))))))))))
