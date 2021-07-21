(defpackage :cp/test/sparse-two-phase-simplex
  (:use :cl :fiveam :cp/sparse-two-phase-simplex :cp/sparse-simplex-common
        :cp/test/nearly-equal :cp/csc :cp/lud :cp/lp-test-tool :cp/shuffle)
  (:import-from :cp/test/base #:base-suite)
  (:import-from :cp/sparse-two-phase-simplex #:tmat-times-vec!))
(in-package :cp/test/sparse-two-phase-simplex)
(in-suite base-suite)

