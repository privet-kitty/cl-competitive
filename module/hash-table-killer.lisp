;;;
;;; Killer against SBCL's hash-table
;;;

;; Copied from sbcl/src/code/target-hash-table.lisp
(declaim (inline prefuzz-hash))
(defun prefuzz-hash (hash)
  ;; We're using power of two tables which obviously are very
  ;; sensitive to the exact values of the low bits in the hash
  ;; value. Do a little shuffling of the value to mix the high bits in
  ;; there too. On 32-bit machines, the result is is a positive fixnum,
  ;; but on 64-bit, we use 2 more bits though must avoid conflict with
  ;; the unique value that that denotes an address-based hash.
  (ldb (byte #-64-bit 29 #+64-bit 31 0)
       (+ (logxor #b11100101010001011010100111 hash)
          (ash hash -3)
          (ash hash -12)
          (ash hash -20))))

;; Randomized attack
(declaim (ftype (function * (values hash-table &optional)) make-killer-sequence))
(defun make-killer-sequence (length
                             &key
                             (max most-positive-fixnum)
                             (test 'eql)
                             (number-of-hashes 3))
  "Generates a killer sequence of non-negative fixnum keys in [0, MAX).
LENGTH must be power of two.

TEST := test function for target hash table
NUMBER-OF-HASHES := the allowed number of hash values"
  (declare (optimize (speed 3))
           (symbol test)
           ((integer 0 #.most-positive-fixnum) number-of-hashes length max))
  (assert (= 1 (logcount length))) ;; power of 2
  (let* ((hash-fn (sb-impl::hash-table-hash-fun (make-hash-table :test test)))
         (table (make-hash-table :test #'eq))
         (mask (- length 1)))
    (dotimes (i length)
      (when (zerop (mod i 1000))
        (format t "~D keys generated.~%" i))
      (loop
        (let* ((x (random max))
               (hash-value (prefuzz-hash (the (integer 0 #.most-positive-fixnum)
                                              (funcall hash-fn x))))
               (masked-value (logand mask hash-value)))
          (when (or (< (hash-table-count table) number-of-hashes)
                    (gethash masked-value table))
            (push x (gethash masked-value table))
            (return)))))
    table))

;; Rule-based attack (utilizing that SBCL's hash-function isn't sensitive to
;; change of high bits)
(declaim (ftype (function * (values list &optional)) make-killer-sequence2))
(defun make-killer-sequence2 (length max)
  "Generates a killer sequence of fixnum keys in [0, MAX). MAX should be
sufficiently large. (say, >= 10^17)"
  (declare ((integer 0 #.most-positive-fixnum) length max))
  (labels ((power-of-two-floor (x) (ash 1 (- (integer-length x) 1))))
    (let* ((max (power-of-two-floor max))
           (min (power-of-two-floor (floor max length))))
      (declare ((integer 0 #.most-positive-fixnum) max min))
      (assert (zerop (mod max min)))
      (loop for i from 1 to length
            collect (* min i)))))
