;; TODO: more efficient handling when modulus is (unsigned-byte 31) or
;; (unsigned-byte 32)
(declaim (inline mod-power))
(defun mod-power (base power modulus)
  "BASE := integer
POWER, MODULUS := non-negative fixnum"
  (declare ((integer 0 #.most-positive-fixnum) modulus power)
           (integer base))
  (if (= modulus 1)
      0
      (labels ((recur (x p)
                 (declare ((integer 0 #.most-positive-fixnum) x p)
                          #+sbcl (values (integer 0 #.most-positive-fixnum)))
                 (cond ((zerop p) 1)
                       ((evenp p) (recur (mod (* x x) modulus) (ash p -1)))
                       (t (mod (* x (recur x (- p 1))) modulus)))))
        (recur (mod base modulus) power))))
