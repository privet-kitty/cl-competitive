;; -*- mode: lisp -*-

;; NOTE: this is a dummy module and contains no other modules.
(defsystem "cp"
  :version "1.0.0"
  :author "Hugo Sansaqua"
  :license "public domain"
  :description "code collection for competitive programming"
  :class :package-inferred-system
  :pathname "module"
  :in-order-to ((test-op (test-op "cp/test"))))

(defsystem "cp/test"
  :depends-on ("fiveam" "cp/test/all")
  :perform (test-op (o c)
                    (uiop:eval-input "(fiveam:run! 'cp/test/base:base-suite)")))
