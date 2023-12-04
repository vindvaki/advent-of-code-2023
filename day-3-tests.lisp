(defpackage #:advent-of-code-2023-tests/day-3-tests
  (:use #:cl #:parachute #:advent-of-code-2023/day-3))

(in-package #:advent-of-code-2023-tests/day-3-tests)

(define-test example-inputs
  (is = 4361 (part-1 *example*))
  (is = 467835 (part-2 *example*)))

(define-test real-inputs
  (is = 539713 (part-1 (load-input)))
  (is = 84159075 (part-2 (load-input))))
