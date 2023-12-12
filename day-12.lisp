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
  (:import-from #:alexandria))

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

(-> map-damage-maps (simple-string list function &key (:start fixnum)) t)
(defun map-damage-maps (partial-damage-map damage-runs fn &key (start 0))
  ;; have we consumed all runs?
  (unless damage-runs
    (funcall fn partial-damage-map)
    (return-from map-damage-maps))
  ;; have we exhausted the map without consuming all runs?
  (when (>= start (length partial-damage-map))
    (return-from map-damage-maps))
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
                      (return-from consume-run)))
             ;; eagerly ensure the end of the run
             (cond
               ((> end (length partial-damage-map)) (return-from consume-run))
               ((< end (length partial-damage-map)) (ecase (aref partial-damage-map end)
                                                      (#\# (reset-and-return))
                                                      (#\. nil)
                                                      (#\? (progn
                                                             (push end changes)
                                                             (setf (aref partial-damage-map end) #\.))))))
             ;; ensure the body of the run
             (dotimes (i run)
               (let* ((pos (+ start i))
                      (c (aref partial-damage-map pos)))
                 (ecase c
                   (#\. (reset-and-return))
                   (#\# nil)
                   (#\? (progn
                          (push pos changes)
                          (setf (aref partial-damage-map pos) #\#))))))
             ;; recurse and reset
             (map-damage-maps partial-damage-map runs fn :start (1+ end))
             (reset)))))
    (ecase (aref partial-damage-map start)
      ;; must consume run
      (#\#
       (consume-run))
      ;; no run to consume
      (#\.
       (map-damage-maps partial-damage-map damage-runs fn :start (1+ start)))
      ;; must consider both options
      (#\?
       (consume-run)
       (setf (aref partial-damage-map start) #\.)
       (map-damage-maps partial-damage-map damage-runs fn :start (1+ start))
       (setf (aref partial-damage-map start) #\?)))))


(defun count-arrangements (partial-damage-map damage-runs)
  (let ((count 0))
    (declare (fixnum count))
    (map-damage-maps partial-damage-map damage-runs (lambda (_)
                                                      (declare (ignore _))
                                                      (incf count)))
    count))

(defun load-input ()
  (read-file-string "day-12.input"))

(defun part-1 (input)
  (loop for (partial-map runs) in (parse-input input)
        summing (count-arrangements partial-map runs)))

(defun unfold-records (partial-map runs)
  (list (string-join (make-list 5 :initial-element partial-map) #\?)
        (append runs runs runs runs runs)))

(defun part-2 (input)
  (loop for (partial-map runs) in (parse-input input)
        for (unfolded-map unfolded-runs) = (unfold-records partial-map runs)
        summing (time (print (count-arrangements unfolded-map unfolded-runs)))))
