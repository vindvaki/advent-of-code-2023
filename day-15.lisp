(defpackage #:advent-of-code-2023/day-15
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
                #:eswitch))

(in-package #:advent-of-code-2023/day-15)

(defparameter *example* "rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7")

(defun elf-hash (string)
  (loop with x = 0
        for c across string
        for i = (char-code c)
        do (setf x (mod (* (+ x i) 17) 256))
        finally (return x)))

(defun load-input ()
  (read-file-string "day-15.input"))

(defun parse-input (input)
  (split "," (trim-whitespace input)))

(defun part-1 (input)
  (loop for s in (parse-input input)
        summing (elf-hash s)))

(defun elf-hashmap-remove (array label)
  (let ((index (elf-hash label)))
    (setf (aref array index)
          (remove-if (lambda (pair) (equal label (car pair)))
                     (aref array index)))))

(defun elf-hashmap-insert (array label focal-length)
  (let ((index (elf-hash label)))
    (dolist (pair (aref array index))
      (when (equal label (car pair))
        (setf (cdr pair) focal-length)
        (return-from elf-hashmap-insert)))
    (push (cons label focal-length) (aref array index))))

(defun focusing-power (array)
  (loop for box from 1
        for list across array
        summing (* box (loop for slot from 1
                             for (label . focal-length) in (reverse list)
                             summing (* slot focal-length)))))

(defun part-2 (input)
  (let ((array (make-array 256 :element-type 'list :initial-element nil)))
    (dolist (instruction (parse-input input))
      (register-groups-bind (label op focal-length-string) ("(\\w+)([-=])(\\d*)" instruction)
        (assert (and label op focal-length-string))
        (eswitch (op :test 'equal)
          ("=" (elf-hashmap-insert array label (parse-integer focal-length-string)))
          ("-" (elf-hashmap-remove array label)))))
    (focusing-power array)))
