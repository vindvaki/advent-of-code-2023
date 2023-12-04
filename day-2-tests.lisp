(defpackage #:advent-of-code-2023-tests/day-2-tests
  (:use #:cl #:parachute #:advent-of-code-2023/day-2))

(in-package #:advent-of-code-2023-tests/day-2-tests)

(define-test example-inputs
  (is = 8 (part-1 *example*))
  (is = 2286 (part-2 *example*)))

(define-test real-inputs
  (is = 1867 (part-1 (load-input)))
  (is = 84538 (part-2 (load-input))))
