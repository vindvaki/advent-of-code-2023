(defpackage #:advent-of-code-2023/day-19
  (:use #:cl)
  (:import-from #:serapeum
                #:dict
                #:take
                #:~>
                #:lines)
  (:import-from #:uiop
                #:read-file-string)
  (:import-from #:alexandria
                #:hash-table-values
                #:last-elt
                #:curry
                #:compose
                #:ensure-gethash
                #:eswitch)
  (:import-from #:cl-ppcre
                #:split
                #:register-groups-bind))

(in-package #:advent-of-code-2023/day-19)

(defparameter *example* "px{a<2006:qkq,m>2090:A,rfg}
pv{a>1716:R,A}
lnx{m>1548:A,A}
rfg{s<537:gd,x>2440:R,A}
qs{s>3448:A,lnx}
qkq{x<1416:A,crn}
crn{x>2662:A,R}
in{s<1351:px,qqz}
qqz{s>2770:qs,m<1801:hdj,R}
gd{a>3333:R,R}
hdj{m>838:A,pv}

{x=787,m=2655,a=1222,s=2876}
{x=1679,m=44,a=2067,s=496}
{x=2036,m=264,a=79,s=2244}
{x=2461,m=1339,a=466,s=291}
{x=2127,m=1623,a=2188,s=1013}")

(defun compile-workflow (string)
  (compile nil `(lambda (x m a s) ,(%compile-workflow string))))

(defun %compile-workflow (string)
  "Compiles the workflow to the equivalent Common Lisp code, but does not compile that code"
  `(symbol-macrolet ,(mapcar (lambda (line)
                               (register-groups-bind (name condition-strings) ("(\\w+)\{(.*)\}" line)
                                 (list (read-from-string name)
                                       (%compile-condition condition-strings))))
                             (lines string))
     in))

(defun %compile-condition (string)
  `(cond ,@(mapcar
            (lambda (branch-string)
              (register-groups-bind (variable ?op ?value ?then) ("(\\w+)([<>])?(\\d+)?:?(\\w+)?" branch-string)
                (if ?op
                    (list (list (read-from-string ?op)
                                (read-from-string variable)
                                (read-from-string ?value))
                          (cond ((string= ?then "A") t)
                                ((string= ?then "R") nil)
                                (t (read-from-string ?then))))
                    (list 't (cond ((string= variable "A") t)
                                   ((string= variable "R") nil)
                                   (t (read-from-string variable)))))))
            (split "," string))))

(defun parse-xmas (string)
  (register-groups-bind ((#'parse-integer x)
                         (#'parse-integer m)
                         (#'parse-integer a)
                         (#'parse-integer s))
      ("\{x=(\\d+),m=(\\d+),a=(\\d+),s=(\\d+)\}" string)
    (list x m a s)))

(defun part-1 (input)
  (destructuring-bind (workflow-string assignment-strings) (split "\\n\\n" input)
    (let ((workflow (compile-workflow workflow-string))
          (assignments (mapcar #'parse-xmas (lines assignment-strings))))
      (loop for (x m a s) in assignments
            when (funcall workflow x m a s)
              sum (+ x m a s)))))

(defun collect-paths (table source &optional (cache (make-hash-table)))
  (when (eql source 't)
    (return-from collect-paths (list nil)))
  (ensure-gethash
   source
   cache
   (serapeum:collecting
     (let ((prefix nil))
       (dolist (clause (gethash source table))
         (destructuring-bind (condition then) clause
           (when (eql condition 't)
             (dolist (subpath (collect-paths table then))
               (collect (append prefix subpath)))
             (return))
           (dolist (subpath (collect-paths table then))
             (collect (append prefix (cons condition subpath))))
           (push (negate-condition condition) prefix)))))))

(defun negate-condition (condition)
  (destructuring-bind (op var val) condition
    (cond ((eql op '<) (list '> var (1- val)))
          ((eql op '>) (list '< var (1+ val))))))

(defun simplify-constraints (constraints)
  (let ((table (make-hash-table)))
    (dolist (var '(x m a s))
      (ensure-gethash var table (cons 1 4000)))
    (prog1 table
      (loop for (op var val) in constraints
            for interval = (gethash var table) do
              (if (eql op '>)
                  (setf (car interval) (max (car interval) (1+ val)))
                  (setf (cdr interval) (min (cdr interval) (1- val))))))))

(defun interval-length (interval)
  (max 0 (1+ (- (cdr interval) (car interval)))))

(defun box-volume (box)
  (reduce #'* (mapcar #'interval-length (hash-table-values box))))

(defun box-intersection (first second)
  (loop with result = (make-hash-table)
        for var in '(x m a s) do
          (setf (gethash var result) (cons (max (car (gethash var first))
                                                (car (gethash var second)))
                                           (min (cdr (gethash var first))
                                                (cdr (gethash var second)))))
        finally (return result)))

(defun part-2 (input)
  (let* ((workflow-string (car (split "\\n\\n" input)))
         (symbolic-conditions (cadr (%compile-workflow workflow-string)))
         (table (make-hash-table)))
    (loop for (name (_ . clauses)) in symbolic-conditions do
      (setf (gethash name table) clauses))
    (loop with boxes = (loop for constraints in (collect-paths table 'in)
                             for box = (simplify-constraints constraints)
                             collect box)
          for box in boxes
          summing (box-volume box)))) ;; the boxes are disjoint by construction
