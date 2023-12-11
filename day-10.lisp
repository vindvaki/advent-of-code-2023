(defpackage #:advent-of-code-2023/day-10
  (:use #:cl)
  (:import-from #:serapeum
                #:->
                #:defconst
                #:queue
                #:~>
                #:lines)
  (:import-from #:uiop
                #:read-file-string)
  (:import-from #:cl-ppcre
                #:split)
  (:import-from #:alexandria
                #:hash-table-values
                #:curry
                #:switch)
  (:export
   #:part-1
   #:load-input
   #:part-2))

(in-package #:advent-of-code-2023/day-10)


(defparameter *example* "7-F7-
.FJ|7
SJLL7
|F--J
LJ.LJ")

(defun parse-input (input)
  (let* ((lines (lines input))
         (rows (length lines))
         (cols (length (car lines))))
    (make-array (list rows cols) :initial-contents (mapcar (lambda (s) (coerce s 'list)) lines))))

(defun grid-position (grid item)
  (destructuring-bind (rows cols) (array-dimensions grid)
    (dotimes (row rows)
      (dotimes (col cols)
        (when (char= item (aref grid row col))
          (return-from grid-position (list row col)))))))

(defconst +up+ '(#\| #\F #\7 #\S))
(defconst +down+ '(#\| #\L #\J #\S))
(defconst +left+ '(#\- #\L #\F #\S))
(defconst +right+ '(#\- #\J #\7 #\S))

(defun allowed-up-down-left-right (value)
  (ecase value
    ;; | is a vertical pipe connecting north and south.
    (#\| (list +up+ +down+ nil nil))
    ;; - is a horizontal pipe connecting east and west.
    (#\- (list nil nil +left+ +right+))
    ;; L is a 90-degree bend connecting north and east.
    (#\L (list +up+ nil nil +right+))
    ;; J is a 90-degree bend connecting north and west.
    (#\J (list +up+ nil +left+ nil))
    ;; 7 is a 90-degree bend connecting south and west.
    (#\7 (list nil +down+ +left+ nil))
    ;; F is a 90-degree bend connecting south and east.
    (#\F (list nil +down+ nil +right+))
    ;; S is the starting position of the animal; there is a pipe on this tile,
    ;; but your sketch doesn't show what shape the pipe has.
    (#\S (list +up+ +down+ +left+ +right+))))

(defun up-down-left-right (pos)
  (destructuring-bind (row col) pos
    (list (list (1- row) col)
          (list (1+ row) col)
          (list row (1- col))
          (list row (1+ col)))))

(defun opposite-direction (direction)
  (mapcar (curry #'* -1) direction))

(-> pipe-neighbors (array list list) list)
(defun pipe-neighbors (grid pos previous-direction)
  (loop with value = (apply #'aref grid pos)
        for allowed in (allowed-up-down-left-right value)
        for neighbor in (up-down-left-right pos)
        for direction in '((-1 0) (+1 0) (0 +1) (0 -1))
        when (and (apply #'array-in-bounds-p grid neighbor)
                  (not (equal direction (opposite-direction previous-direction)))
                  (member (apply #'aref grid neighbor) allowed :test #'char=))
          collect (list neighbor direction)))

(defun find-loop (grid source)
  (do ((stack (list (list source nil nil)))
       (visited (make-hash-table :test 'equal)))
      ((not stack))
    (destructuring-bind (pos path direction) (pop stack)
      (setf (gethash pos visited) t)
      (dolist (neighbor-direction (pipe-neighbors grid pos direction))
        (destructuring-bind (neighbor direction) neighbor-direction
          (when (equal neighbor source)
            (return-from find-loop (cons pos path)))
          (unless (gethash neighbor visited)
            (push (list neighbor (cons pos path) direction) stack)))))))

(defun load-input ()
  (read-file-string "day-10.input"))

(defun part-1 (input)
  (let* ((grid (parse-input input))
         (source (grid-position grid #\S)))
    (ceiling (length (find-loop grid source)) 2)))


;; Idea: The loop is a polygon. To test if a point is in polygon, we can cast a
;; ray outwards and count the parity of hits to the loop boundary. If it's odd,
;; we are in the interior, if it's even we are in the exterior.
;;
;; But there is a catch: This does not work if the ray is a superset of an edge.
;; The probability of this is typically zero, but in our case it would be nice
;; to just cast vertical or horizontal rays, which actually fully contain edges.
;;
;; To solve this, we can do a simple trick: Shift the ray's origin by 1/2 on the
;; axis orthogonal to the ray. This will ensure the ray never contains an edge,
;; making the parity trick work again.
;;
;; Next is making the intersection efficient. Let's say we'll always cast
;; vertical rays upwards. Then we index the loop boundary's horizontal
;; mid-points, keeping for each "half column" the sorted array of mid-points
;; along it. Each ray cast can then be performed in O(log(n)) time with binary
;; search, though I will probably just opt for O(n) because I expect n to be
;; small (then we can also skip the sorting).

(defun part-2 (input)
  (let* ((grid (parse-input input))
         (source (grid-position grid #\S))
         (loop-path (find-loop grid source))
         (loop-table (make-hash-table :test 'equal))
         (index (make-hash-table))
         (result 0))
    ;; prepare loop table for fast lookup
    (dolist (pos loop-path)
      (setf (gethash pos loop-table) t))
    ;; prepare half-column index
    (loop for (edge-begin edge-end-or-nil) on loop-path
          for edge-end = (or edge-end-or-nil (car loop-path))
          for mid = (~> (mapcar #'+ edge-begin edge-end)
                        (mapcar (lambda (x) (/ x 2)) _))
          for col = (cadr mid)
          when (/= 0 col)
            do (push mid (gethash col index nil)))
    ;; cast half-column rays
    (destructuring-bind (rows cols) (array-dimensions grid)
      (dotimes (row rows)
        (dotimes (col cols)
          (unless (gethash (list row col) loop-table)
            (let ((intersections (loop for (ray-row ray-col) in (gethash (+ col 1/2) index)
                                       counting (> ray-row row))))
              (when (oddp intersections)
                (incf result)))))))
    result))

(defparameter *large-example* ".F----7F7F7F7F-7....
.|F--7||||||||FJ....
.||.FJ||||||||L7....
FJL7L7LJLJ||LJ.L-7..
L--J.L7...LJS7F-7L7.
....F-J..F7FJ|L7L7L7
....L7.F7||L7|.L7L7|
.....|FJLJ|FJ|F7|.LJ
....FJL-7.||.||||...
....L---J.LJ.LJLJ...")
