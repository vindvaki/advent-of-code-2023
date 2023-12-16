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
                #:reducing
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
    (labels ((recurse (&rest next-directions)
               (dolist (next-direction next-directions)
                 (trace-beam grid (mapcar #'+ position next-direction) next-direction visited))))
      (eswitch ((apply #'aref grid position))
        (#\. (recurse direction))
        (#\| (if (= 0 (cadr direction))
                 (recurse direction)
                 (recurse '(-1 0) '(+1 0))))
        (#\- (if (= 0 (car direction))
                 (recurse direction)
                 (recurse '(0 -1) '(0 +1))))
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

(defun energized-count (grid &optional (position '(0 0)) (direction '(0 +1)))
  (hash-table-count (trace-beam grid position direction)))

(defun part-1 (input)
  (let* ((grid (parse-grid input)))
    (energized-count grid)))

(defun load-input ()
  (read-file-string "day-16.input"))

(defun part-2 (input)
  (let ((grid (parse-grid input)))
    (reducing (#'max maximize)
      (destructuring-bind (rows cols) (array-dimensions grid)
        (dotimes (row rows)
          (maximize (energized-count grid (list row 0) '(0 +1)))
          (maximize (energized-count grid (list row 0) '(0 -1))))
        (dotimes (col cols)
          (maximize (energized-count grid (list 0 col) '(+1 0)))
          (maximize (energized-count grid (list (1- rows) col) '(-1 0))))))))
