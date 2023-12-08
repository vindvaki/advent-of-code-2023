(defpackage #:advent-of-code-2023-tests/day-6-tests
  (:use #:cl #:parachute #:advent-of-code-2023/day-6))

(in-package #:advent-of-code-2023-tests/day-6-tests)

(define-test example-inputs
  (is = 288 (part-1 *example*))
  (is = 71503 (part-2 *example*)))

(define-test real-inputs
  (is = 3317888 (part-1 (load-input)))
  (is = 24655068 (part-2 (load-input))))
