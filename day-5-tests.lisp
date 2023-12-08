(defpackage #:advent-of-code-2023-tests/day-5-tests
  (:use #:cl #:parachute #:advent-of-code-2023/day-5))

(in-package #:advent-of-code-2023-tests/day-5-tests)

(define-test example-inputs
  (is = 35 (part-1 *example*))
  (is = 46 (part-2 *example*)))

(define-test real-inputs
  (is = 650599855 (part-1 (load-input)))
  (is = 1240035 (part-2 (load-input))))
