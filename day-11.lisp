(defpackage #:advent-of-code-2023/day-11
  (:use #:cl)
  (:import-from #:serapeum
                #:~>
                #:lines)
  (:import-from #:uiop
                #:read-file-string)
  (:import-from #:cl-ppcre
                #:split)
  (:import-from #:alexandria
                #:map-combinations))

(in-package #:advent-of-code-2023/day-11)

(defparameter *example* "...#......
.......#..
#.........
..........
......#...
.#........
.........#
..........
.......#..
#...#.....")

(defun character-list (string)
  (coerce string 'list))

(defun parse-input (input)
  (let* ((lines (lines input))
         (rows (length lines))
         (cols (length (car lines))))
    (make-array (list rows cols) :initial-contents (mapcar #'character-list lines))))

(defun expanded-galaxies (grid &optional (expansion-factor 1))
  (destructuring-bind (rows cols) (array-dimensions grid)
    (let* ((empty-rows (loop for row from 0 upto (1- rows)
                             when (loop for col from 0 upto (1- cols)
                                        always (char= #\. (aref grid row col)))
                               collect row))
           (empty-cols (loop for col from 0 upto (1- cols)
                             when (loop for row from 0 upto (1- rows)
                                        always (char= #\. (aref grid row col)))
                               collect col))
           (length-empty-rows (length empty-rows))
           (length-empty-cols (length empty-cols))
           (actual-factor (max 1 (1- expansion-factor)))
           (result nil))
      (dotimes (row rows)
        (dotimes (col cols)
          (when (char= #\# (aref grid row col))
            (push (list (+ row (* actual-factor (or (position row empty-rows :test #'<) length-empty-rows)))
                        (+ col (* actual-factor (or (position col empty-cols :test #'<) length-empty-cols))))
                  result))))
      result)))

(defun manhattan-distance (xs ys)
  (loop for x in xs
        for y in ys
        summing (abs (- y x))))

(defun load-input ()
  (read-file-string "day-11.input"))

(defun part-1 (input)
  (solve input 1))

(defun solve (input &optional (expansion-factor 1))
  (let* ((grid (parse-input input))
         (result 0)
         (expanded-galaxies (expanded-galaxies grid expansion-factor)))
    (map-combinations (lambda (pair)
                        (incf result (apply #'manhattan-distance pair)))
                      expanded-galaxies :length 2)
    result))

(defun part-2 (input)
  (solve input 1000000))
