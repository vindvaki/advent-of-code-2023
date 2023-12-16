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
  (eswitch ((apply #'aref grid position))
    (#\. (trace-beam grid (mapcar #'+ position direction) direction visited))
    (#\| (if (= 0 (cadr direction))
             (trace-beam grid (mapcar #'+ position direction) direction visited)
             (progn
               (trace-beam grid (mapcar #'+ '(-1 0) position) '(-1 0) visited)
               (trace-beam grid (mapcar #'+ '(+1 0) position) '(+1 0) visited)
               visited)))
    (#\- (if (= 0 (car direction))
             (trace-beam grid (mapcar #'+ position direction) direction visited)
             (progn
               (trace-beam grid (mapcar #'+ '(0 -1) position) '(0 -1) visited)
               (trace-beam grid (mapcar #'+ '(0 +1) position) '(0 +1) visited)
               visited)))
    (#\\ (eswitch (direction :test 'equal)
           ('(+1 0) (trace-beam grid (mapcar #'+ position '(0 +1)) '(0 +1) visited))
           ('(-1 0) (trace-beam grid (mapcar #'+ position '(0 -1)) '(0 -1) visited))
           ('(0 +1) (trace-beam grid (mapcar #'+ position '(+1 0)) '(+1 0) visited))
           ('(0 -1) (trace-beam grid (mapcar #'+ position '(-1 0)) '(-1 0) visited))))
    (#\/ (eswitch (direction :test 'equal)
           ('(+1 0) (trace-beam grid (mapcar #'+ position '(0 -1)) '(0 -1) visited))
           ('(-1 0) (trace-beam grid (mapcar #'+ position '(0 +1)) '(0 +1) visited))
           ('(0 +1) (trace-beam grid (mapcar #'+ position '(-1 0)) '(-1 0) visited))
           ('(0 -1) (trace-beam grid (mapcar #'+ position '(+1 0)) '(+1 0) visited))))))

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
