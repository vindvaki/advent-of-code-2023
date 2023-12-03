(defpackage #:advent-of-code-2023/day-2
  (:use #:cl)
  (:import-from #:serapeum
                #:~>
                #:dict
                #:lines)
  (:import-from #:uiop
                #:read-file-string)
  (:import-from #:alexandria
                #:hash-table-values)
  (:import-from #:cl-ppcre
                #:register-groups-bind
                #:split)
  (:export
   #:*example*
   #:load-input
   #:part-1
   #:part-2))

(in-package #:advent-of-code-2023/day-2)


(defparameter *example* "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green")

(defparameter *limits*
  (dict "red" 12
        "green" 13
        "blue" 14))

(defun load-input ()
  (read-file-string "day-2.input"))

(defun parse-game (string)
  (register-groups-bind ((#'parse-integer id) (#'parse-draws draws)) ("Game (\\d+): (.*)" string)
    (list id draws)))

(defun parse-draws (string)
  (~> (split "; " string)
      (mapcar #'parse-draw _)))

(defun parse-draw (string &aux (result nil))
  (dolist (count-color-string (split ", " string) result)
    (destructuring-bind (count-string color) (split " " count-color-string)
      (let ((count (parse-integer count-string)))
        (push (list count color) result)))))

(defun part-1 (input)
  (loop with result = 0
        for line in (lines input)
        for (id draws) = (parse-game line)
        when (loop for draw in draws
                   always (loop for (count color) in draw
                                always (<= count (gethash color *limits*))))
          summing id))

(defun part-2 (input)
  (loop for line in (lines input)
        for (id draws) = (parse-game line)
        summing (loop with maximums = (make-hash-table :test 'equal)
                      for draw in draws do
                        (loop for (count color) in draw do
                          (setf (gethash color maximums) (max (gethash color maximums 0) count)))
                      finally (return (reduce #'* (hash-table-values maximums))))))
