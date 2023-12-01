(defsystem "advent-of-code-tests"
  :class :package-inferred-system
  :author "Hordur Freyr Yngvason"
  :license "MIT"
  :depends-on ("advent-of-code-tests/tests")
  :perform (test-op (op c)
                    (symbol-call :advent-of-code-tests/tests :run)))
