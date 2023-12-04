(uiop:define-package #:advent-of-code-2023-tests/tests
  (:use #:cl)
  (:import-from #:parachute)
  (:import-from #:advent-of-code-2023-tests/day-1-tests)
  (:import-from #:advent-of-code-2023-tests/day-2-tests)
  (:import-from #:advent-of-code-2023-tests/day-3-tests)
  (:import-from #:advent-of-code-2023-tests/day-4-tests)
  (:export #:run))

(in-package #:advent-of-code-2023-tests/tests)

(defun run (&optional (packages (parachute:test-packages)))
  "Entrypoint for `asdf:test-system'"
  (parachute:test packages))
