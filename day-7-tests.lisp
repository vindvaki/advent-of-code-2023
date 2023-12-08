(defpackage #:advent-of-code-2023-tests/day-7-tests
  (:use #:cl #:parachute #:advent-of-code-2023/day-7))

(in-package #:advent-of-code-2023-tests/day-7-tests)

(define-test example-inputs
  (is = 6440 (part-1 *example*))
  (is = 5905 (part-2 *example*)))

(define-test real-inputs
  (is = 250957639 (part-1 (load-input)))
  (is = 251515496 (part-2 (load-input))))
