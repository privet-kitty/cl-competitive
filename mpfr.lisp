(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; FIXME: kludge for AtCoder
  (unless (or (probe-file #P"/usr/lib/x86_64-linux-gnu/libmpfr.so")
              (probe-file #P"/usr/lib/x86_64-linux-gnu/libmpfr.so.4"))
    (load-shared-object "libmpfr.so.6" :dont-save t))
  (require :sb-mpfr)
  (add-package-local-nickname :mpfr :sb-mpfr))
(eval-when (:compile-toplevel :load-toplevel :execute)
  (mpfr:set-precision 64))
