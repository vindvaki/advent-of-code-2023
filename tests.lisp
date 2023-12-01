(uiop:define-package #:advent-of-code-2023-tests/tests
  (:use #:cl)
  (:import-from #:parachute)
  (:export #:run))

(in-package #:advent-of-code-2023-tests/tests)

(defun run (&optional (packages (parachute:test-packages)))
  "Entrypoint for `asdf:test-system'"
  (parachute:test packages))
