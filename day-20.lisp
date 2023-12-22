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

(defun broadcast (table &key (broadcaster-name "broadcaster") sink-name)
  (do* ((broadcaster (gethash broadcaster-name table))
        (queue (queue (list broadcaster :low nil)))
        (pulses nil))
       ((queue-empty-p queue) pulses)
    (destructuring-bind (module pulse-type parent) (deq queue)
      (let ((name (pulse-module-name module)))
        (push (cons name pulse-type) pulses)
        (when (and (eql :low pulse-type) (equal sink-name name))
          (return pulses))
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
            (pulse-module (propagate pulse-type))))))))

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
      (dolist (pulse-type (broadcast table))
        (ecase pulse-type
          (:low (incf total-low-pulse-count))
          (:high (incf total-high-pulse-count)))))
    (values (* total-low-pulse-count total-high-pulse-count) total-low-pulse-count total-high-pulse-count)))

;;
;; Part 2
;;
;; So this one seems like I have to look at my input. Let's visualize with graphviz:

(defun render (input stream)
  (let ((table (parse-input input)))
    (labels ((pulse-module-typestring (name)
               (etypecase (gethash name table)
                 (flip-flop-module "\\%")
                 (conjunction-module "\&")
                 (pulse-module "")))
             (typed-pulse-module-name (name)
               (format nil "~A~A" (pulse-module-typestring name) name)))
      (write-string "digraph {" stream)
      (do-hash-table (name module table)
        (when-let ((outputs (mapcar #'typed-pulse-module-name (pulse-module-outputs module))))
          (format stream "~S -> {~{~S~^  ~}}~%" (typed-pulse-module-name name) outputs)))
      (write-string "}" stream))))

(defun render-input-to-file (path)
  (with-open-file (stream path :direction :output
                               :if-exists :supersede
                               :if-does-not-exist :create)
    (render (read-file-string "day-20.input") stream)))

;; I see that rx has a single input &nr, which itself just has inputs &mm, &ff,
;; &fk and &lh.
;;
;; Each of those looks like the sink of an independent subgraph. More precisely, each is
;; connected with a single edge from the broadcaster, and has a single edge out to one
;; of the above conjunction modules.
;;
;; I have already established that a brute force of the entire thing is
;; infeasible, but we might be able to find the activation cycles of each
;; subgraph. Then, rx receives a low pulse when all subgraphs fire a high pulse
;; simultaneously.
;;
;; To find the activation cycles, I've modified the `broadcast' function to
;; return a "pulse signature" for the button push, and to accept an optional
;; sink name to target the subgraphs individually.
;;
;; I'm also going to guess that the activation of each sink is exactly at the
;; end of a cycle, because that already happened once this AoC and it would be
;; the simplest to code (otherwise we would need to align the offsets).

(defun find-low-activation (table broadcaster-name sink-name)
  (loop for i from 0
        for key = (broadcast table :broadcaster-name broadcaster-name :sink-name sink-name)
        when (equal (cons sink-name :low) (car key))
          return i))


(defun part-2 (input)
  (let* ((table (parse-input input))
         (source-sinks '(("cn" "fk")
                         ("kh" "ff")
                         ("pf" "mm")
                         ("sj" "lh")))
         (activations (mapcar (lambda (pair) (apply #'find-low-activation table pair)) source-sinks)))
    (apply #'lcm (mapcar #'1+ activations))))
