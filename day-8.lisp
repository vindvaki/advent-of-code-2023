(defpackage #:advent-of-code-2023/day-8
  (:use #:cl)
  (:import-from #:serapeum
                #:enq
                #:deq
                #:queue-empty-p
                #:queue
                #:~>
                #:lines)
  (:import-from #:uiop
                #:read-file-string)
  (:import-from #:cl-ppcre
                #:register-groups-bind
                #:split)
  (:import-from #:alexandria
                #:when-let
                #:last-elt
                #:eswitch
                #:hash-table-values)
  (:export
   #:*example*))

(in-package #:advent-of-code-2023/day-8)

(defparameter *example* "RL

AAA = (BBB, CCC)
BBB = (DDD, EEE)
CCC = (ZZZ, GGG)
DDD = (DDD, DDD)
EEE = (EEE, EEE)
GGG = (GGG, GGG)
ZZZ = (ZZZ, ZZZ)")

(defparameter *example-2* "LLR

AAA = (BBB, BBB)
BBB = (AAA, ZZZ)
ZZZ = (ZZZ, ZZZ)")

(defun load-input ()
  (read-file-string "day-8.input"))

(defun parse-input (input)
  (let* ((table (make-hash-table :test 'equal))
         (lines (lines input))
         (instructions (pop lines)))
    (pop lines) ; empty line
    (dolist (line lines)
      (register-groups-bind (source left right) ("(\\w+)\\s+=\\s+\\((\\w+),\\s+(\\w+)\\)" line)
        (setf (gethash source table) (list left right))))
    (values instructions table)))

(defun follow-instructions (table instructions source target)
  (loop with n = (length instructions)
        with node = source
        for i = 0 then (1+ i)
        for instruction = (aref instructions (rem i n))  do
          (progn
            (when (equal node target)
              (return i))
            (let ((children (gethash node table)))
              (setf node (if (char= instruction #\L)
                             (car children)
                             (cadr children)))))))

(defun part-1 (input)
  (multiple-value-bind (instructions table) (parse-input input)
    (follow-instructions table instructions "AAA" "ZZZ")))

(defparameter *example-3* "LR

11A = (11B, XXX)
11B = (XXX, 11Z)
11Z = (11B, XXX)
22A = (22B, XXX)
22B = (22C, 22C)
22C = (22Z, 22Z)
22Z = (22B, 22B)
XXX = (XXX, XXX)")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Part 2
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; We need to concurrently follow multiple paths until all of them are terminals
;; at the same time. Given how small the input is, and how long ther brute force
;; was taking on my computer, it's likely there are loops, and we can assume the
;; concurrent terminals occur in the loop.
;;
;; This means we have k loops with lengths n_1 ... n_k and we can reduce the problem
;; to finding the smallest x such that
;;
;; x = a_1 (mod n_1), ..., x = a_k (mod n_k)
;;
;; for some combination of terminal indexes a_1 ... a_k. Note that the terminal
;; indexes must be calculated relative to a shared starting point, which is the
;; furthest distance from source_i to terminal_i.
;;
;; If n_1 ... n_k are pairwise coprime, then this can be solved with the chinese
;; remainder theorem for each combination of terminal indexes.
;;
;; So let's start by detecting the cycle lengths:

(defun find-cycle (table instructions source)
  "Returns three values: The the cycle length, the distance traveled to the cycle
at `source', and the starting node"
  (loop with n = (length instructions)
        with node = source
        with first-visited-at = (make-hash-table :test 'equal)
        for i = 0 then (1+ i)
        for instruction = (aref instructions (rem i n))
        for key = (cons node (rem i n)) do
          (progn
            (when-let ((j (gethash key first-visited-at)))
              (return (values (- i j) j node)))
            (setf (gethash key first-visited-at) i)
            (let ((children (gethash node table)))
              (setf node (if (char= instruction #\L)
                             (car children)
                             (cadr children)))))))

;; My investigation reveals that the cycle lengths in my input are not pairwise
;; coprime, but they all share the common factor 277, which is the length of the
;; instruction string. This means we can move forward with a _modified_ chinese
;; remainder theorem approach:
;;
;;
;; x = a_1 (mod n_1), ..., x = a_k (mod n_k)
;; x = a_1 (mod 277), ..., x = a_k (mod 277)
;;
;; where n_1, ... n_k are the cycle lengths divided by 277.
;;
;; The problem is now to identify the terminals:

(defun terminal-distances (table instructions source offset max-distance)
  "Returns the distances to all terminals reachable from `source' within
`max-distance' (inclusive)"
  (loop with n = (length instructions)
        with node = source
        for i = offset then (1+ i)
        for d = 0 then (1+ d)
        for instruction = (aref instructions (rem i n))
        when (char= #\Z (last-elt node))
          collect d
        while (<= d max-distance) do
          (progn
            (print node)
            (let ((children (gethash node table)))
              (setf node (if (char= instruction #\L)
                             (car children)
                             (cadr children)))))))


;; Ok, this is stupid, but looking at my input, I saw that AAA only had one
;; terminal within its cycle length, and it was exactly the last element in the
;; cycle. I guessed that the same might hold for all cycles, in which case the
;; solution is the least-common-multiple of all cycle lengths, calculated this
;; in the REPL and it was accepted by the online judge!
;;
;; I don't think this property is guaranteed to hold for all inputs, but I don't
;; feel like writing up a proper solution, so I'll just leave part 2 uncoded.
