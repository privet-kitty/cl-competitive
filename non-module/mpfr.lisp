(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; FIXME: hack for Ubuntu 18.04 (e.g. AtCoder)
  #+linux
  (unless (or (probe-file #P"/usr/lib/x86_64-linux-gnu/libmpfr.so")
              (probe-file #P"/usr/lib/x86_64-linux-gnu/libmpfr.so.4"))
    (sb-alien:load-shared-object
     (file-namestring (car (directory #P"/usr/lib/x86_64-linux-gnu/libmpfr*.*")))))
  ;; Assumes https://github.com/emphasis87/libmpfr-msys2-mingw64
  #+win32 (sb-alien:load-shared-object "libmpfr-4.dll")
  (let ((*break-on-signals* nil))
    (require :sb-mpfr))
  (sb-ext:add-package-local-nickname :mpfr :sb-mpfr))
(eval-when (:compile-toplevel :load-toplevel :execute)
  (mpfr:set-precision 64))
