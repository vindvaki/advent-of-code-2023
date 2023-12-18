(defpackage #:advent-of-code-2023/day-18
  (:use #:cl)
  (:import-from #:serapeum
                #:take
                #:~>
                #:lines)
  (:import-from #:uiop
                #:read-file-string)
  (:import-from #:alexandria
                #:curry
                #:compose
                #:eswitch)
  (:import-from #:cl-ppcre
                #:register-groups-bind))

(in-package #:advent-of-code-2023/day-18)

(defparameter *example* "R 6 (#70c710)
D 5 (#0dc571)
L 2 (#5713f0)
D 2 (#d2c081)
R 2 (#59c680)
D 2 (#411b91)
L 5 (#8ceee2)
U 2 (#caa173)
L 1 (#1b58a2)
U 2 (#caa171)
R 2 (#7807d2)
U 3 (#a77fa3)
L 2 (#015232)
U 2 (#7a21e3)")

(defun parse-direction (string)
  (eswitch (string :test #'string=)
    ("D" '(+1 0))
    ("U" '(-1 0))
    ("L" '(0 -1))
    ("R" '(0 +1))))

(defun parse-line (line)
  (register-groups-bind ((#'parse-direction direction) (#'parse-integer amount) hexcolor)
      ("^(D|L|R|U) (\\d+) \\((.*)\\)$" line)
    (list direction amount hexcolor)))

(defun parse-input (input)
  (mapcar #'parse-line (lines input)))

(defun list+ (a b)
  (mapcar #'+ a b))

(defun scale (a x)
  (mapcar (curry #'* a) x))

(defun trace-path (instructions)
  (let* ((pos (list 0 0))
         (path (list pos)))
    (dolist (instruction instructions path)
      (destructuring-bind (direction amount) (take 2 instruction)
        (setf pos (list+ pos (scale amount direction)))
        (push pos path)))))

(defun expand-path (path)
  "Expands the path to a real number polygon that properly contains the path's
coordinates on an integer pixel grid."
  (loop with orientation = (polygon-orientation path)
        with expanded = (list (copy-list (car path)))
        for (p ?q) on path
        for q = (or ?q (car path))
        for u = (mapcar #'- q p)
        for v = (mapcar #'signum u)
        when ?q
          do (cond
               ;; if we're walking
               ;;   - up and the interior is on the left
               ;;   - down and the interior is on the right
               ;; then expand the edge to the right
               ((= (car v) (- orientation)) (progn (incf (cadr (car expanded)))
                                                   (push (list (car q) (1+ (cadr q))) expanded)))
               ;; if we're walking
               ;;   - right and the interior is on the left
               ;;   - left and the interior is on the right
               ;; then expand the edge down
               ((= (cadr v) orientation) (progn (incf (car (car expanded)))
                                                (push (list (1+ (car q)) (cadr q)) expanded)))
               (t (push q expanded)))
        finally (return expanded)))

(defun polygon-orientation (path)
  "Returns +1 if the polygon is positively oriented, i.e. its interior is on the
left of each edge, and -1 if it is negatively oriented."
  (signum (%polygon-area path)))

(defun polygon-area (path)
  (abs (%polygon-area path)))

(defun %polygon-area (path)
  (/ (loop for (p ?q) on path
           for q = (or ?q (car path))
           for (px py) = p
           for (qx qy) = q
           summing (* (+ py qy) (- px qx)))
     2))

(defun part-1 (input)
  (let* ((instructions (parse-input input))
         (path (expand-path (trace-path instructions))))
    (polygon-area path)))

(defun load-input ()
  (read-file-string "day-18.input"))

(defun parse-hex-instructions (instructions)
  (mapcar (compose #'parse-hex-instruction #'third) instructions))

(defun parse-hex-instruction (hex)
  (let* ((amount (parse-integer (subseq hex 1 6) :radix 16))
         (direction-number (parse-integer (subseq hex 6) :radix 16))
         (direction (nth direction-number '((0 +1) (+1 0) (0 -1) (-1 0)))))
    (list direction amount)))

(defun part-2 (input)
  (let* ((instructions (parse-input input))
         (hex-instructions (parse-hex-instructions instructions))
         (path (expand-path (trace-path hex-instructions))))
    (polygon-area path)))
