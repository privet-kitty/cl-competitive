;; On SBCL version earlier than 1.5.6 this type deriver is necessary for
;; MAKE-ARRAY to propagate the element type of a multi-dimensional array. It is
;; a dead copy of the commit
;; https://github.com/sbcl/sbcl/commit/c84daa0791f4d4ed03469fc23a1d1bc2aadacda2
(eval-when (:compile-toplevel :load-toplevel :execute)
  (sb-c:defoptimizer (sb-c::make-array-header* sb-c::derive-type) ((&rest inits))
    (let* ((data-position #.(sb-vm::slot-offset
                             (find 'sb-vm::data (sb-vm:primitive-object-slots
                                                 (find 'array sb-vm:*primitive-objects*
                                                       :key 'sb-vm:primitive-object-name))
                                   :key 'sb-vm::slot-name)))
           (data (nth data-position inits))
           (type (sb-c::lvar-type data)))
      (when (sb-kernel:array-type-p type)
        (sb-kernel:make-array-type '*
                                   :element-type (sb-kernel:array-type-element-type type)
                                   :specialized-element-type (sb-kernel:array-type-specialized-element-type type))))))
