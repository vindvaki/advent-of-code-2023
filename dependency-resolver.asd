(defsystem "dependency-resolver"
  :class :package-inferred-system
  :version "0.1.0"
  :author "Hordur Freyr Yngvason"
  :license "MIT"
  :depends-on (:dependency-resolver/dependency-resolver)
  :description "A minimal dependency resolver"
  :build-operation "program-op"
  :build-pathname "dependency-resolver"
  :entry-point "dependency-resolver/dependency-resolver:main")
