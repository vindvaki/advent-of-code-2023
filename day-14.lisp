(defpackage #:advent-of-code-2023/day-14
  (:use #:cl)
  (:import-from #:serapeum
                #:~>
                #:lines)
  (:import-from #:uiop
                #:read-file-string)
  (:import-from #:cl-ppcre
                #:split)
  (:import-from #:alexandria
                #:copy-array
                #:when-let
                #:eswitch
                #:ensure-gethash)
  (:import-from #:advent-of-code-2023/utils
                #:parse-grid
                #:print-grid))

(in-package #:advent-of-code-2023/day-14)

(defparameter *example* "O....#....
O.OO#....#
.....##...
OO.#O....O
.O.....O#.
O.#..O.#.#
..O..#O..O
.......O..
#....###..
#OO..#....")

(defun parse-input (input)
  (parse-grid input))

(defun tilt-north (grid)
  (loop with (rows cols) = (array-dimensions grid)
        for col from 0 upto (1- cols) do
          (loop for row-0 from 0 upto (1- rows)
                when (char= #\O (aref grid row-0 col)) do
                  (loop for row from row-0 downto 1
                        while (char= #\. (aref grid (1- row) col)) do
                          (rotatef (aref grid row col)
                                   (aref grid (1- row) col)))))
  grid)

(defun rock-load (grid)
  (loop with (rows cols) = (array-dimensions grid)
        for row from 0 upto (1- rows)
        summing (loop for col from 0 upto (1- cols)
                      when (char= #\O (aref grid row col))
                        summing (- rows row))))

(defun load-input ()
  (read-file-string "day-14.input"))

(defun part-1 (input)
  (~> (parse-input input)
      (tilt-north _)
      (rock-load _)))

(defun transpose (grid)
  (destructuring-bind (rows cols) (array-dimensions grid)
    (assert (= rows cols)) ;; in-place transpose is only correct for square matrices
    (dotimes (row rows)
      (dotimes (col row)
        (rotatef (aref grid row col)
                 (aref grid col row)))))
  grid)

(defun mirror-columns (grid)
  (loop with (rows cols) = (array-dimensions grid)
        for row below rows do
          (loop for col below (floor cols 2) do
            (rotatef (aref grid row col)
                     (aref grid row (- cols col 1)))))
  grid)

(defun rotate-clockwise (grid)
  (~> (transpose grid)
      (mirror-columns _)))

(defun spin-once (grid)
  (dotimes (i 4)
    (tilt-north grid)
    (rotate-clockwise grid))
  grid)

(defun spin-times (grid-0 count)
  (do* ((grid->index (make-hash-table :test 'equalp))
        (index->grid (make-hash-table))
        (grid grid-0)
        (index 0))
       ((= index count) grid)
    (when-let ((first-index (gethash grid grid->index)))
      (return (~> (mod (- count first-index) (- index first-index))
                  (+ first-index _)
                  (gethash _ index->grid))))
    (setf (gethash grid grid->index) index
          (gethash index index->grid) grid)
    (setf grid (spin-once (copy-array grid)))
    (incf index)))

(defun part-2 (input)
  (~> (parse-input input)
      (spin-times _ 1000000000)
      (rock-load _)))
