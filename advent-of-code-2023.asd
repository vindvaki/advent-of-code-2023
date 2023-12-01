(defsystem "advent-of-code-2023"
  :class :package-inferred-system
  :version "0.1.0"
  :author "Hordur Freyr Yngvason"
  :license "MIT"
  :depends-on (:advent-of-code-2023/main)
  :description "My solutions to Advent of Code 2023"
  :in-order-to ((test-op (test-op :advent-of-code-2023-tests))))
