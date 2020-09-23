(defpackage :cp/test/prefix-p
  (:use :cl :fiveam :cp/prefix-p)
  (:import-from :cp/test/base #:base-suite))
(in-package :cp/test/prefix-p)
(in-suite base-suite)

(test prefix-p
  (is-true (prefix-p "asd" "asdf"))
  (is-true (prefix-p "asdf" "asdf"))
  (is-true (prefix-p "" "asdf"))
  (is-true (prefix-p "" ""))
  (is-false (prefix-p "ase" "asdf"))
  (is-false (prefix-p "asdff" "asdf")))

(test suffix-p
  (is-true (suffix-p "sdf" "asdf"))
  (is-true (suffix-p "asdf" "asdf"))
  (is-true (suffix-p "" "asdf"))
  (is-true (suffix-p "" ""))
  (is-false (suffix-p "tdf" "asdf"))
  (is-false (suffix-p "aasdf" "asdf")))
