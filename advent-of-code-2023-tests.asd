(defsystem "advent-of-code-2023-tests"
  :class :package-inferred-system
  :author "Hordur Freyr Yngvason"
  :license "MIT"
  :depends-on ("advent-of-code-2023-tests/tests")
  :perform (test-op (op c)
                    (symbol-call :advent-of-code-2023-tests/tests :run))
  :build-operation "program-op"
  :build-pathname "run-tests"
  :entry-point "advent-of-code-2023-tests/tests:run")
