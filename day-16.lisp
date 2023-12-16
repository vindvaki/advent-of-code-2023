(defpackage #:advent-of-code-2023/day-16
  (:use #:cl)
  (:import-from #:serapeum
                #:trim-whitespace
                #:~>
                #:lines)
  (:import-from #:uiop
                #:read-file-string)
  (:import-from #:cl-ppcre
                #:split
                #:register-groups-bind)
  (:import-from #:alexandria
                #:when-let
                #:eswitch)
  (:import-from #:advent-of-code-2023/utils
                #:parse-grid))

(in-package #:advent-of-code-2023/day-16)

(defparameter *example* ".|...\\....
|.-.\\.....
.....|-...
........|.
..........
.........\\
..../.\\\\..
.-.-/..|..
.|....-|.\\
..//.|....")

(defun trace-beam (grid &optional
                          (position '(0 0))
                          (direction '(0 +1))
                          (visited (make-hash-table :test 'equal)))
  (when (or (member direction (gethash position visited) :test 'equal)
            (not (apply #'array-in-bounds-p grid position)))
    (return-from trace-beam visited))
  (push direction (gethash position visited))
  (prog1 visited
    (labels ((recurse (next-direction)
               (trace-beam grid (mapcar #'+ position next-direction) next-direction visited)))
      (eswitch ((apply #'aref grid position))
        (#\. (recurse direction))
        (#\| (if (= 0 (cadr direction))
                 (recurse direction)
                 (progn
                   (recurse '(-1 0))
                   (recurse '(+1 0)))))
        (#\- (if (= 0 (car direction))
                 (recurse direction)
                 (progn
                   (recurse '(0 -1))
                   (recurse '(0 +1)))))
        (#\\ (eswitch (direction :test 'equal)
               ('(+1 0) (recurse '(0 +1)))
               ('(-1 0) (recurse '(0 -1)))
               ('(0 +1) (recurse '(+1 0)))
               ('(0 -1) (recurse '(-1 0)))))
        (#\/ (eswitch (direction :test 'equal)
               ('(+1 0) (recurse '(0 -1)))
               ('(-1 0) (recurse '(0 +1)))
               ('(0 +1) (recurse '(-1 0)))
               ('(0 -1) (recurse '(+1 0)))))))))

(defun part-1 (input)
  (let* ((grid (parse-grid input))
         (visited (trace-beam grid)))
    (hash-table-count visited)))

(defun load-input ()
  (read-file-string "day-16.input"))

(defun part-2 (input)
  (let ((grid (parse-grid input))
        (result 0))
    (labels ((maximize (row col direction)
               (let ((visited (trace-beam grid (list row col) direction)))
                 (setf result (max result (hash-table-count visited))))))
      (destructuring-bind (rows cols) (array-dimensions grid)
        (dotimes (row rows)
          (maximize row 0 '(0 +1))
          (maximize row (1- cols) '(0 -1)))
        (dotimes (col cols)
          (maximize 0 col '(+1 0))
          (maximize (1- rows) col '(-1 0)))))
    result))
