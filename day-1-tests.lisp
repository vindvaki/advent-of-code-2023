(defpackage #:advent-of-code-2023-tests/day-1-tests
  (:use #:cl #:parachute #:advent-of-code-2023/day-1)
  (:export #:run))

(in-package #:advent-of-code-2023-tests/day-1-tests)

(define-test example-inputs
  (is = 142 (part-1 *example*))
  (is = 281 (part-2 *example-2*)))

(define-test real-inputs
  (is = 54304 (part-1 (load-input)))
  (is = 54418 (part-2 (load-input))))
