;;;
;;; ARRAY-ELEMENT-TYPE is not constant-folded on SBCL version earlier than
;;; 1.5.0. See
;;; https://github.com/sbcl/sbcl/commit/9f0d12e7ab961828931d01c0b2a76a5885ad35d2
;;;

(eval-when (:compile-toplevel :load-toplevel :execute)
  (sb-c:deftransform array-element-type ((array))
    (let ((type (sb-c::lvar-type array)))
      (flet ((element-type (type)
               (and (sb-c::array-type-p type)
                    (sb-int:neq (sb-kernel::array-type-specialized-element-type type) sb-kernel:*wild-type*)
                    (sb-kernel:type-specifier (sb-kernel::array-type-specialized-element-type type)))))
        (cond ((let ((type (element-type type)))
                 (and type
                      `',type)))
              ((sb-kernel:union-type-p type)
               (let (result)
                 (loop for type in (sb-kernel:union-type-types type)
                       for et = (element-type type)
                       unless (and et
                                   (if result
                                       (equal result et)
                                       (setf result et)))
                       do (sb-c::give-up-ir1-transform))
                 `',result))
              ((sb-kernel:intersection-type-p type)
               (loop for type in (sb-kernel:intersection-type-types type)
                     for et = (element-type type)
                     when et
                     return `',et
                     finally (sb-c::give-up-ir1-transform)))
              (t
               (sb-c::give-up-ir1-transform)))))))
