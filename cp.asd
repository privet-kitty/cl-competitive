;; -*- mode: lisp -*-

(defsystem "cp"
  :version "0.1.0"
  :author "Hugo Sansaqua"
  :license "public domain"
  :description "code collection for competitive programming"
  :class :package-inferred-system
  :pathname "src"
  :depends-on ("cp/all")
  :in-order-to ((test-op (test-op "cp/test"))))

(defsystem "cp/test"
  :depends-on ("fiveam" "cp/test/main")
  :perform (test-op (o c)
                    (uiop:eval-input "(fiveam:run! 'cp/test/main:main-suite)")))
