(defpackage #:dependency-resolver/dependency-resolver
  (:use #:cl)
  (:import-from #:serapeum
                #:~>)
  (:import-from #:alexandria
                #:hash-table-keys)
  (:import-from #:uiop
                #:command-line-arguments)
  (:export
   #:main))

(in-package #:dependency-resolver/dependency-resolver)

(defun main ()
  (print (mapcar #'all-dependencies (command-line-arguments))))

(defun normalize-dependency (dep)
  (etypecase dep
    (string (string-downcase dep))
    (symbol (normalize-dependency (symbol-name dep)))
    (list
     (let ((dep-type (first dep)))
       (normalize-dependency
        (ecase dep-type
          (:version (second dep))
          (:feature (third dep))
          (:require (second dep))))))))

(defun sideways-dependencies (system)
  (ignore-errors
   (~> (asdf:find-system system)
       (asdf:component-sideway-dependencies _)
       (mapcar #'normalize-dependency _))))

(defun all-dependencies (system)
  (do ((stack (list system))
       (visited (make-hash-table :test 'equal)))
      ((not stack) (sort (hash-table-keys visited) #'string<=))
    (let* ((current (pop stack))
           (children (sideways-dependencies current)))
      (setf (gethash current visited) t)
      (dolist (child children)
        (unless (gethash child visited)
          (push child stack))))))
