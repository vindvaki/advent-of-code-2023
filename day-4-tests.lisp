(defpackage #:advent-of-code-2023-tests/day-4-tests
  (:use #:cl #:parachute #:advent-of-code-2023/day-4))

(in-package #:advent-of-code-2023-tests/day-4-tests)

(define-test example-inputs
  (is = 13 (part-1 *example*))
  (is = 30 (part-2 *example*)))

(define-test real-inputs
  (is = 24706 (part-1 (load-input)))
  (is = 13114317 (part-2 (load-input))))
