(defpackage #:advent-of-code-2023/day-20
  (:use #:cl)
  (:import-from #:serapeum
                #:queue-empty-p
                #:enq
                #:deq
                #:queue
                #:do-hash-table
                #:dict
                #:take
                #:~>
                #:lines)
  (:import-from #:uiop
                #:read-file-string)
  (:import-from #:alexandria
                #:when-let
                #:hash-table-values
                #:last-elt
                #:curry
                #:compose
                #:ensure-gethash
                #:eswitch)
  (:import-from #:cl-ppcre
                #:split
                #:register-groups-bind))

(in-package #:advent-of-code-2023/day-20)

(defparameter *example* "broadcaster -> a, b, c
%a -> b
%b -> c
%c -> inv
&inv -> a")

(defclass pulse-module ()
  ((name :initarg :name
         :reader pulse-module-name)
   (inputs :initform nil
           :accessor pulse-module-inputs)
   (outputs :initarg :outputs
            :initform nil
            :reader pulse-module-outputs)))

(defclass flip-flop-module (pulse-module)
  ((state :initarg :state
          :initform nil
          :reader flip-flop-module-state
          :type boolean)))

(defun toggle-flip-flop-module-state (module)
  (with-slots (state) module
    (setf state (not state))))

(defclass conjunction-module (pulse-module)
  ((memory :initarg :memory
           :initform (make-hash-table :test 'equal)
           :reader conjunction-module-memory
           :type hash-table)))

(defun broadcast (table)
  (do* ((broadcaster (gethash "broadcaster" table))
        (low-pulse-count 0)
        (high-pulse-count 0)
        (queue (queue (list broadcaster :low nil))))
       ((queue-empty-p queue) (values low-pulse-count high-pulse-count nil))
    (destructuring-bind (module pulse-type parent) (deq queue)
      (ecase pulse-type
        (:low (incf low-pulse-count))
        (:high (incf high-pulse-count)))
      (labels ((propagate (next-pulse-type)
                 (dolist (out (pulse-module-outputs module))
                   (enq (list (gethash out table) next-pulse-type module) queue))))
        (etypecase module
          (flip-flop-module
           (when (eql pulse-type :low)
             (propagate (if (toggle-flip-flop-module-state module)
                            :high :low))))
          (conjunction-module
           (setf (gethash (pulse-module-name parent) (conjunction-module-memory module)) pulse-type)
           (propagate (if (loop for input-name in (pulse-module-inputs module)
                                always (eql :high (gethash input-name (conjunction-module-memory module) :low)))
                          :low :high)))
          (pulse-module (propagate pulse-type)))))))

(defun parse-pulse-module (string)
  (register-groups-bind (?type name ((curry #'split ", ") outputs))
      ("([&%]?)(\\w+) -> (.*)" string)
    (eswitch (?type :test 'equal)
      ("" (make-instance 'pulse-module :name name :outputs outputs))
      ("%" (make-instance 'flip-flop-module :name name :outputs outputs))
      ("&" (make-instance 'conjunction-module :name name :outputs outputs)))))

(defun parse-input (input)
  (let ((table (make-hash-table :test 'equal)))
    (prog1 table
      (dolist (line (lines input))
        (let ((module (parse-pulse-module line)))
          (setf (gethash (pulse-module-name module) table) module)))
      (do-hash-table (name module table)
        (dolist (out-name (pulse-module-outputs module))
          (let ((out (ensure-gethash out-name table (make-instance 'pulse-module :name out-name))))
            (push name (pulse-module-inputs out))))))))

(defmethod print-object ((module pulse-module) stream)
  (print-unreadable-object (module stream :type t :identity t)
    (format stream "~A -> ~{~A~^, ~}"
            (pulse-module-name module)
            (pulse-module-outputs module))))

(defun part-1 (input &optional (count 1000))
  (let ((table (parse-input input))
        (total-low-pulse-count 0)
        (total-high-pulse-count 0))
    (dotimes (_ count)
      (multiple-value-bind (low-pulse-count high-pulse-count) (broadcast table)
        (incf total-low-pulse-count low-pulse-count)
        (incf total-high-pulse-count high-pulse-count)))
    (values (* total-low-pulse-count total-high-pulse-count) total-low-pulse-count total-high-pulse-count)))
