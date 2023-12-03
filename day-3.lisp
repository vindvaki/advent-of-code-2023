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

(defstruct (number-interval (:constructor make-number-interval (number row col-begin col-end)))  number row col-begin col-end)

(defun parse-input (input)
  (let* ((lines (lines input))
         (rows (length lines))
         (cols (length (car lines)))
         (grid (make-array (list rows cols) :initial-contents (mapcar (lambda (s) (coerce s 'list)) lines)))
         (number-intervals (make-number-intervals grid)))
    (values grid number-intervals)))

(defun make-number-intervals (grid)
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
                    (collect (make-number-interval number row digit-begin col))
                    (setf digit-begin nil
                          number 0)))))
          ;; end-of-row needs special handling
          (when digit-begin
            (collect (make-number-interval number row digit-begin cols))))))))

(defun symbol-adjacent-p (grid interval)
  (destructuring-bind (rows cols) (array-dimensions grid)
    (let ((irow (number-interval-row interval))
          (icols-begin (number-interval-col-begin interval))
          (icols-end (number-interval-col-end interval)))
      (loop :for row :from (1- irow) :upto (1+ irow) :do
        (when (<= 0 row (1- rows)) ;; in the grid
          (loop :for col :from (1- icols-begin) :upto icols-end :do
            (when (and (<= 0 col (1- cols)) ;; in the grid
                       (not (and (= row irow) (<= icols-begin col (1- icols-end)))) ;; not in the current interval
                       (char/= #\. (aref grid row col))) ;; is a symbol
              (return-from symbol-adjacent-p t))))))))

(defun load-input ()
  (read-file-string "day-3.input"))

(defun part-1 (input)
  (multiple-value-bind (grid intervals) (parse-input input)
    (summing
      (dolist (interval intervals)
        (when (symbol-adjacent-p grid interval)
          (sum (number-interval-number interval)))))))

(defun interval-neighbor-p (interval row col)
  (let ((irow (number-interval-row interval))
        (icol-begin (number-interval-col-begin interval))
        (icol-end (number-interval-col-end interval)))
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
                                     (collect (number-interval-number interval)))))))
                (when (= 2 (length neighbors))
                  (sum (reduce #'* neighbors)))))))))))
