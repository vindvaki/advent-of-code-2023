(defpackage #:advent-of-code-2023/day-5
  (:use #:cl)
  (:import-from #:serapeum
                #:collecting
                #:batches
                #:drop-suffix
                #:split-sequence
                #:trim-whitespace
                #:summing
                #:~>
                #:lines)
  (:import-from #:uiop
                #:read-file-string)
  (:import-from #:cl-ppcre
                #:register-groups-bind
                #:split)
  (:import-from #:alexandria
                #:ensure-gethash))

(in-package #:advent-of-code-2023/day-5)

(defparameter *example* "seeds: 79 14 55 13

seed-to-soil map:
50 98 2
52 50 48

soil-to-fertilizer map:
0 15 37
37 52 2
39 0 15

fertilizer-to-water map:
49 53 8
0 11 42
42 0 7
57 7 4

water-to-light map:
88 18 7
18 25 70

light-to-temperature map:
45 77 23
81 45 19
68 64 13

temperature-to-humidity map:
0 69 1
1 0 69

humidity-to-location map:
60 56 37
56 93 4")

(multiple-value-bind (s m) (parse-input *example*)
  (defparameter *seeds* s)
  (defparameter *map* m))

(defun parse-input (input)
  (let* ((lines (lines input))
         (seeds (parse-seeds (car lines)))
         (maps (parse-maps (cddr lines))))
    (values seeds maps)))

(defun parse-seeds (string)
  (~> (split " " string)
      (cdr _)
      (mapcar #'parse-integer _)))

(defun parse-maps (lines)
  (let ((table (make-hash-table :test 'equal)))
    (dolist (map-lines (split-sequence "" lines :test 'equal) table)
      (multiple-value-bind (key value) (parse-map map-lines)
        (setf (gethash (car key) table) (cons (cadr key) value))))))

(defun car<= (a b)
  (<= (car a) (car b)))

(defun parse-map (lines)
  (let ((key (split "-to-" (drop-suffix " map:" (car lines))))
        (value nil))
    (dolist (line (cdr lines) (values key (sort value #'car<=)))
      (let ((numbers (~> (split " " line)
                         (mapcar #'parse-integer _))))
        (push numbers value)))))

(defun load-input ()
  (read-file-string "day-5.input"))

(defun translate-number (source source-to-target-ranges)
  (loop for (target-begin source-begin range-length) in source-to-target-ranges
        when (<= source-begin source (+ source-begin range-length)) do
          (return (+ target-begin (- source source-begin)))))

(defun trace-seed-to-location (seed map)
  (do* ((key "seed")
        (number seed))
       ((string= key "location") number)
    (let* ((key-ranges (gethash key map))
           (translated (translate-number number (cdr key-ranges))))
      (setf key (car key-ranges)
            number (if translated translated number)))))

(defun part-1 (input)
  (multiple-value-bind (seeds map) (parse-input input)
    (loop for seed in seeds
          for location = (trace-seed-to-location seed map)
          minimize location)))

(defun empty-interval-p (interval)
  (< (cadr interval) (car interval)))

(defun interval-intersection (a b)
  (unless (and a b)
    (return-from interval-intersection))
  (if (> (car a) (car b))
      (interval-intersection b a)
      (destructuring-bind (a0 a1) a
        (destructuring-bind (b0 b1) b
          (when (<= b0 a1)
            (list (max a0 b0)
                  (min a1 b1)))))))

(defun interval-difference (a b)
  (let ((x (interval-intersection a b)))
    (if x
        (delete-if #'empty-interval-p
                   (list (list (car a) (1- (car x)))
                         (list (1+ (cadr x)) (cadr a))))
        (list a))))

(defun interval-union (intervals)
  (collecting
    (do* ((iter (sort intervals #'car<=))
          (current (copy-list (car iter))))
         ((not (cdr iter)) (collect current))
      (setf iter (cdr iter))
      (let ((next (car iter)))
        (if (interval-intersection current next)
            (setf (cadr current) (max (cadr current) (cadr next)))
            (progn (collect current)
                   (setf current (copy-list next))))))))

(defun translate-interval (source-interval source-to-target-ranges)
  (interval-union
   (loop with result = nil
         with remaining = (list source-interval)
         for range in source-to-target-ranges
         for (target-begin range-source-begin range-length) = range
         for range-source-interval = (list range-source-begin (+ range-source-begin range-length)) do
           (loop with next-remaining = nil
                 for interval in remaining
                 for intersection = (interval-intersection interval range-source-interval)
                 do (progn
                      (when intersection
                        (push (list (+ target-begin (- (car intersection) range-source-begin))
                                    (+ target-begin (- (cadr intersection) range-source-begin)))
                              result))
                      (setf next-remaining (nconc next-remaining (interval-difference interval intersection))))
                 finally (setf remaining next-remaining))
         finally (return (nconc result remaining)))))

(defun translate-intervals (source-intervals source-to-target-ranges)
  (interval-union
   (loop for interval in source-intervals
         appending (translate-interval interval source-to-target-ranges))))

(defun trace-seed-intervals-to-location (seed-intervals map)
  (do* ((key "seed")
        (intervals seed-intervals))
       ((string= key "location") intervals)
    (let* ((key-ranges (gethash key map)))
      (setf key (car key-ranges)
            intervals (translate-intervals intervals (cdr key-ranges))))))

(defun range-to-interval (range)
  (destructuring-bind (begin length) range
    (list begin (+ begin length))))

(defun part-2 (input)
  (multiple-value-bind (seed-ranges map) (parse-input input)
    (let* ((result nil)
           (seed-intervals (mapcar #'range-to-interval (batches seed-ranges 2)))
           (location-intervals (trace-seed-intervals-to-location seed-intervals map)))
      (dolist (location-interval location-intervals result)
        (if result
            (setf result (min result (car location-interval)))
            (setf result (car location-interval)))))))
