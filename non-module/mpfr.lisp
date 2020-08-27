(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; FIXME: kludge for AtCoder
  #+linux
  (unless (or (probe-file #P"/usr/lib/x86_64-linux-gnu/libmpfr.so")
              (probe-file #P"/usr/lib/x86_64-linux-gnu/libmpfr.so.4"))
    (sb-alien:load-shared-object "libmpfr.so.6" :dont-save t))
  #+win32 (sb-alien:load-shared-object "libmpfr-4.dll")
  (require :sb-mpfr)
  (sb-ext:add-package-local-nickname :mpfr :sb-mpfr))
(eval-when (:compile-toplevel :load-toplevel :execute)
  (mpfr:set-precision 64))
