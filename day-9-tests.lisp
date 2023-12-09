(defpackage #:advent-of-code-2023-tests/day-9-tests
  (:use #:cl #:parachute #:advent-of-code-2023/day-9))

(in-package #:advent-of-code-2023-tests/day-9-tests)

(define-test example-inputs
  (is = 114 (part-1 *example*))
  (is = 2 (part-2 *example*)))

(define-test real-inputs
  (is = 1708206096 (part-1 (load-input)))
  (is = 1050 (part-2 (load-input))))
