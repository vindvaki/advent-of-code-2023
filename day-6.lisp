(defpackage #:advent-of-code-2023/day-6
  (:use #:cl)
  (:import-from #:serapeum
                #:string+
                #:~>
                #:lines)
  (:import-from #:uiop
                #:read-file-string)
  (:import-from #:cl-ppcre
                #:split)
  (:export
   #:load-input
   #:part-1
   #:part-2))

(in-package #:advent-of-code-2023/day-6)


(defparameter *example* "Time:      7  15   30
Distance:  9  40  200")

(defun parse-input-1 (input)
  (mapcar #'parse-line-1 (lines input)))

(defun parse-line-1 (line)
  (~> (split " +" line)
      (cdr _)
      (mapcar #'parse-integer _)))

(defun distance (acceleration-time total-time)
  (* acceleration-time (- total-time acceleration-time)))

;; This is a quadratic equation:
;;
;; -x*x + x*t > d
;; -x*x + x*t - d > 0
;; x*x - x*t + d < 0
;;
;; other constraints are 0 <= x <= t
;;
;; it's 0 at x = (/ 2 (+/- t (sqrt (- (* t t)  (* 4 d))))
(defun winning-range (time distance)
  (let ((a (max 0 (ceiling (- time (sqrt (- (* time time) (* 4 distance)))) 2)))
        (b (min time (floor (+ time (sqrt (- (* time time) (* 4 distance)))) 2))))
    (list (if (> (distance a time) distance) a (1+ a))
          (if (> (distance b time) distance) b (1- b)))))

(defun load-input ()
  (read-file-string "day-6.input"))

(defun part-1 (input)
  (destructuring-bind (times distances) (parse-input-1 input)
    (loop for time in times
          for distance in distances
          for (a b) = (winning-range time distance)
          for range-length = (1+ (- b a))
          for result = range-length then (* range-length result)
          finally (return result))))

(defun parse-input-2 (input)
  (mapcar #'parse-line-2 (lines input)))

(defun parse-line-2 (line)
  (~> (split " +" line)
      (cdr _)
      (apply #'string+ _)
      (parse-integer _)))

(defun part-2 (input)
  (destructuring-bind (time distance) (parse-input-2 input)
    (destructuring-bind (a b) (winning-range time distance)
      (1+ (- b a)))))
