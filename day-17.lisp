(defpackage #:advent-of-code-2023/day-17
  (:use #:cl)
  (:import-from #:serapeum
                #:collecting
                #:heap-insert
                #:make-heap
                #:heap-maximum
                #:heap-extract-maximum
                #:~>
                #:lines)
  (:import-from #:uiop
                #:read-file-string)
  (:import-from #:alexandria
                #:eswitch)
  (:import-from #:advent-of-code-2023/utils
                #:reducing))

(in-package #:advent-of-code-2023/day-17)

(defparameter *example* "2413432311323
3215453535623
3255245654254
3446585845452
4546657867536
1438598798454
4457876987766
3637877979653
4654967986887
4564679986453
1224686865563
2546548887735
4322674655533")

(defun parse-input (input)
  (let* ((lines (lines input))
         (rows (length lines))
         (cols (length (car lines)))
         (digits (loop for line in lines
                       collect (loop for char across line
                                     collect (- (char-code char) (char-code #\0))))))
    (make-array (list rows cols) :initial-contents digits)))

(defun left-right (direction)
  (eswitch (direction :test 'equal)
    ('(+1 0) '((0 -1) (0 +1)))
    ('(-1 0) '((0 +1) (0 -1)))
    ('(0 -1) '((-1 0) (+1 0)))
    ('(0 +1) '((+1 0) (-1 0)))))

(defun list+ (xs ys)
  (mapcar #'+ xs ys))

(defstruct state
  position direction budget cost)

(defun state-cost<= (first second)
  (<= (state-cost first)
      (state-cost second)))

(defun minimal-heat-loss (grid &optional (source '(0 0)) (target (list
                                                                  (1- (array-dimension grid 0))
                                                                  (1- (array-dimension grid 1)))))
  (let ((heap (make-heap :size (reduce #'* (array-dimensions grid))
                         :test #'state-cost<=))
        (table (make-hash-table :test 'equal))
        (target-keys (loop for budget from 0 upto 2
                           appending (loop for direction in '((+1 0) (0 +1))
                                           collecting (list target direction budget)))))
    (labels ((try-insert-next-state (position direction budget cost)
               (when (> budget 0)
                 (let ((next-position (list+ position direction)))
                   (when (apply #'array-in-bounds-p grid next-position)
                     (heap-insert
                      heap
                      (make-state
                       :position next-position
                       :direction direction
                       :budget (1- budget)
                       :cost (+ (apply #'aref grid next-position) cost))))))))
      (reducing (#'min minimize)
        (try-insert-next-state source '(+1 0) 3 0)
        (try-insert-next-state source '(0 +1) 3 0)
        (do ()
            ((not (heap-maximum heap)) nil)
          (let ((state (heap-extract-maximum heap)))
            (with-slots (position direction budget cost) state
              (let ((key (list position direction budget)))
                (when (< cost (gethash key table (1+ cost)))
                  (setf (gethash key table) cost)
                  (when (member key target-keys :test 'equal)
                    (minimize cost)
                    (setf target-keys (delete key target-keys :test 'equal))
                    (unless target-keys
                      (return)))
                  (dolist (next-direction (left-right direction))
                    (try-insert-next-state position next-direction 3 cost))
                  (try-insert-next-state position direction budget cost))))))))))

(defun part-1 (input)
  (minimal-heat-loss (parse-input input)))

(defun load-input ()
  (read-file-string "day-17.input"))

(defstruct (ultra-state (:include state)) run)

(defun ultra-minimal-heat-loss (grid &optional (source '(0 0)) (target (list
                                                                        (1- (array-dimension grid 0))
                                                                        (1- (array-dimension grid 1)))))
  (let ((heap (make-heap :size (reduce #'* (array-dimensions grid))
                         :test #'state-cost<=))
        (table (make-hash-table :test 'equal))
        (target-keys (make-hash-table :test 'equal)))
    (loop for run from 0 upto (reduce #'max (array-dimensions grid)) do
      (loop for budget below 10 do
        (loop for direction in '((+1 0) (0 +1)) do
          (setf (gethash (list target direction run budget) target-keys) t))))
    (labels ((try-insert-next-state (position direction run budget cost)
               (when (> budget 0)
                 (let ((next-position (list+ position direction)))
                   (when (apply #'array-in-bounds-p grid next-position)
                     (heap-insert
                      heap
                      (make-ultra-state
                       :position next-position
                       :direction direction
                       :run (1+ run)
                       :budget (1- budget)
                       :cost (+ (apply #'aref grid next-position) cost))))))))
      (reducing (#'min minimize)
        (try-insert-next-state source '(+1 0) 0 10 0)
        (try-insert-next-state source '(0 +1) 0 10 0)
        (do ()
            ((not (heap-maximum heap)) nil)
          (let ((state (heap-extract-maximum heap)))
            (with-slots (position direction run budget cost) state
              (let ((key (list position direction run budget)))
                (when (< cost (gethash key table (1+ cost)))
                  (setf (gethash key table) cost)
                  (when (gethash key target-keys)
                    (minimize cost)
                    (remhash key target-keys)
                    (when (= 0 (hash-table-count target-keys))
                      (return)))
                  (when (>= run 4)
                    (dolist (next-direction (left-right direction))
                      (try-insert-next-state position next-direction 0 10 cost)))
                  (try-insert-next-state position direction run budget cost))))))))))

(defun part-2 (input)
  (ultra-minimal-heat-loss (parse-input input)))
