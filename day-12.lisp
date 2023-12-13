(defpackage #:advent-of-code-2023/day-12
  (:use #:cl)
  (:import-from #:serapeum
                #:->
                #:string-join
                #:collecting
                #:~>
                #:lines)
  (:import-from #:uiop
                #:read-file-string)
  (:import-from #:cl-ppcre
                #:split)
  (:import-from #:alexandria
                #:ensure-gethash))

(in-package #:advent-of-code-2023/day-12)

(defparameter *example* "???.### 1,1,3
.??..??...?##. 1,1,3
?#?#?#?#?#?#?#? 1,3,1,6
????.#...#... 4,1,1
????.######..#####. 1,6,5
?###???????? 3,2,1")

(defun parse-input (input)
  (~> (lines input)
      (mapcar #'parse-line _)))

(defun parse-line (line)
  (destructuring-bind (partial-damage-map damage-runs-string) (split " " line)
    (let ((runs (~> (split "," damage-runs-string)
                    (mapcar #'parse-integer _))))
      (list partial-damage-map runs))))

(-> damage-runs (simple-string))
(defun damage-runs (partial-damage-map)
  (collecting
    (let ((run 0))
      (declare (fixnum run))
      (dotimes (i (length partial-damage-map))
        (if (char= #\# (aref partial-damage-map i))
            (incf run)
            (progn
              (when (> run 0)
                (collect run))
              (setf run 0))))
      (when (> run 0)
        (collect run)))))

(-> count-damage-maps (simple-string list &key (:start fixnum) (:cache hash-table)) fixnum)
(defun count-damage-maps (partial-damage-map damage-runs &key (start 0) (cache (make-hash-table :test 'equal)))
  (ensure-gethash
   (list damage-runs start)
   cache
   (block nil
     ;; have we consumed all runs?
     (unless damage-runs
       (return (if (or (>= start (length partial-damage-map))
                       (not (position #\# partial-damage-map :start start)))
                   1 0)))
     ;; have we exhausted the map without consuming all runs?
     (when (>= start (length partial-damage-map))
       (return 0))
     ;; recursive case
     (labels
         ((consume-run ()
            (let* ((runs damage-runs)
                   (run (pop runs))
                   (changes nil)
                   (end (+ start run)))
              (labels ((reset ()
                         (push run runs)
                         (dolist (pos changes)
                           (setf (aref partial-damage-map pos) #\?)))
                       (reset-and-return ()
                         (reset)
                         (return-from consume-run 0)))
                ;; eagerly ensure the end of the run
                (cond
                  ((> end (length partial-damage-map)) (reset-and-return))
                  ((< end (length partial-damage-map)) (ecase (aref partial-damage-map end)
                                                         (#\# (reset-and-return))
                                                         (#\. nil)
                                                         (#\? (progn
                                                                (push end changes)
                                                                (setf (aref partial-damage-map end) #\.))))))
                ;; ensure the body of the run
                (dotimes (i run)
                  (let ((pos (+ start i)))
                    (ecase (aref partial-damage-map pos)
                      (#\. (reset-and-return))
                      (#\# nil)
                      (#\? (progn
                             (push pos changes)
                             (setf (aref partial-damage-map pos) #\#))))))
                ;; recurse and reset
                (prog1 (count-damage-maps partial-damage-map runs :start (1+ end) :cache cache)
                  (reset))))))
       (ecase (aref partial-damage-map start)
         ;; must consume run
         (#\#
          (consume-run))
         ;; no run to consume
         (#\.
          (count-damage-maps partial-damage-map damage-runs :start (1+ start) :cache cache))
         ;; must consider both options
         (#\?
          (+ (consume-run)
             (count-damage-maps partial-damage-map damage-runs :start (1+ start) :cache cache))))))))

(defun load-input ()
  (read-file-string "day-12.input"))

(defun part-1 (input)
  (loop for (partial-map runs) in (parse-input input)
        summing (count-damage-maps partial-map runs)))

(defun unfold-records (partial-map runs)
  (list (string-join (make-list 5 :initial-element partial-map) #\?)
        (append runs runs runs runs runs)))

(defun part-2 (input)
  (loop for (partial-map runs) in (parse-input input)
        for (unfolded-map unfolded-runs) = (unfold-records partial-map runs)
        summing (count-damage-maps unfolded-map unfolded-runs)))
