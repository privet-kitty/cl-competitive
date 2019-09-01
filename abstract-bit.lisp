;;;
;;; 1-dimensional binary indexed tree
;;;

(defmacro define-bitree (name &key (operator '#'+) (identity 0) sum-type (order '#'>))
  "OPERATOR := binary operator (on a commutative monoid)
IDENTITY := object (identity element of the monoid)
ORDER := nil | strict comparison operator on the monoid
SUM-TYPE := nil | type specifier

Defines no structure; BIT is just a vector. This macro defines the three
function: <NAME>-UPDATE!, <NAME>-SUM and COERCE-TO-<NAME>!. If ORDER is
specified, this macro defines the bisection function <NAME>-BISECT-LEFT in
addition. (Note that the -BISECT-LEFT function works only when the sequence of
prefix sums (VECTOR[0], VECTOR[0]+VECTOR[1], ...) is monotone.)

SUM-TYPE is used only for the type declaration: each sum
VECTOR[i]+VECTOR[i+1]...+VECTOR[i+k] is declared to be this type. (The
element-type of vector itself doesn't need to be SUM-TYPE.)"
  (let* ((name (string name))
         (fname-update (intern (format nil "~A-UPDATE!" name)))
         (fname-sum (intern (format nil "~A-SUM" name)))
         (fname-coerce (intern (format nil "COERCE-TO-~A!" name)))
         (fname-bisect-left (intern (format nil "~A-BISECT-LEFT" name)))
         (fname-bisect-right (intern (format nil "~A-BISECT-RIGHT" name))))
    `(progn
       (declaim (inline ,fname-update))
       (defun ,fname-update (bitree index delta)
         "Destructively increments the vector: vector[INDEX] = vector[INDEX] +
DELTA"
         (let ((len (length bitree)))
           (do ((i index (logior i (+ i 1))))
               ((>= i len) bitree)
             (declare ((integer 0 #.most-positive-fixnum) i))
             (setf (aref bitree i)
                   (funcall ,operator (aref bitree i) delta)))))

       (declaim (inline ,fname-sum))
       (defun ,fname-sum (bitree end)
         "Returns the sum of the prefix: vector[0] + ... + vector[END-1]."
         (declare ((integer 0 #.most-positive-fixnum) end))
         (let ((res ,identity))
           ,@(when sum-type `((declare (type ,sum-type res))))
           (do ((i (- end 1) (- (logand i (+ i 1)) 1)))
               ((< i 0) res)
             (declare ((integer -1 #.most-positive-fixnum) i))
             (setf res (funcall ,operator res (aref bitree i))))))

       (declaim (inline ,fname-coerce))
       (defun ,fname-coerce (vector)
         "Destructively constructs BIT from VECTOR."
         (loop with len = (length vector)
               for i below len
               for dest-i = (logior i (+ i 1))
               when (< dest-i len)
               do (setf (aref vector dest-i)
                        (funcall ,operator (aref vector dest-i) (aref vector i)))
               finally (return vector)))

       ,@(when order
           `((declaim (inline ,fname-bisect-left))
             (defun ,fname-bisect-left (bitree value)
               "Returns the smallest index that fulfills VECTOR[0]+ ... +
VECTOR[index] >= VALUE. Returns the length of VECTOR if VECTOR[0]+
... +VECTOR[length-1] < VALUE."
               (declare (vector bitree))
               (if (not (funcall ,order value ,identity))
                   0
                   (let ((len (length bitree))
                         (index+1 0)
                         (cumul ,identity))
                     (declare ((integer 0 #.most-positive-fixnum) index+1)
                              ,@(when sum-type
                                  `((type ,sum-type cumul))))
                     (do ((delta (ash 1 (- (integer-length len) 1))
                                 (ash delta -1)))
                         ((zerop delta) index+1)
                       (declare ((integer 0 #.most-positive-fixnum) delta))
                       (let ((next-index (+ index+1 delta -1)))
                         (when (< next-index len)
                           (let ((next-cumul (funcall ,operator cumul (aref bitree next-index))))
                             ,@(when sum-type
                                 `((declare (type ,sum-type next-cumul))))
                             (when (funcall ,order value next-cumul)
                               (setf cumul next-cumul)
                               (incf index+1 delta)))))))))
             (declaim (inline ,fname-bisect-right))
             (defun ,fname-bisect-right (bitree value)
               "Returns the smallest index that fulfills VECTOR[0]+ ... +
VECTOR[index] > VALUE. Returns the length of VECTOR if VECTOR[0]+
... +VECTOR[length-1] <= VALUE."
               (declare (vector bitree))
               (if (funcall ,order ,identity value)
                   0
                   (let ((len (length bitree))
                         (index+1 0)
                         (cumul ,identity))
                     (declare ((integer 0 #.most-positive-fixnum) index+1)
                              ,@(when sum-type
                                  `((type ,sum-type cumul))))
                     (do ((delta (ash 1 (- (integer-length len) 1))
                                 (ash delta -1)))
                         ((zerop delta) index+1)
                       (declare ((integer 0 #.most-positive-fixnum) delta))
                       (let ((next-index (+ index+1 delta -1)))
                         (when (< next-index len)
                           (let ((next-cumul (funcall ,operator cumul (aref bitree next-index))))
                             ,@(when sum-type
                                 `((declare (type ,sum-type next-cumul))))
                             (unless (funcall ,order next-cumul value)
                               (setf cumul next-cumul)
                               (incf index+1 delta))))))))))))))

(define-bitree bitree
  :operator #'+
  :identity 0
  :sum-type fixnum
  :order #'>)

;; Example: inversion number
;; (declaim (inline make-reverse-lookup-table))
;; (defun make-reverse-lookup-table (vector &key (test #'eql))
;;   "Assigns each value of the (usually sorted) VECTOR of length n to the integers
;; 0, ..., n-1."
;;   (let ((table (make-hash-table :test test :size (length vector))))
;;     (dotimes (i (length vector) table)
;;       (setf (gethash (aref vector i) table) i))))

;; (defun calc-inversion-number (vector &key (test #'<))
;;   (declare (vector vector))
;;   (let* ((len (length vector))
;;          (rev-lookup-table (make-reverse-lookup-table (sort (copy-seq vector) test)))
;;          (bitree (make-array len :element-type '(integer 0 #.most-positive-fixnum)))
;;          (inversion-number 0))
;;     (declare (integer inversion-number))
;;     (loop for j below len
;;           for element = (aref vector j)
;;           for compressed = (gethash element rev-lookup-table)
;;           for delta of-type integer = (- j (bitree-sum bitree (1+ compressed)))
;;           do (incf inversion-number delta)
;;              (bitree-update! bitree compressed 1))
;;     inversion-number))

;; (progn
;;   (assert (= 3 (calc-inversion-number #(2 4 1 3 5))))
;;   (assert (zerop (calc-inversion-number #(0))))
;;   (assert (zerop (calc-inversion-number #())))
;;   (assert (zerop (calc-inversion-number #(1 2))))
;;   (assert (= 1 (calc-inversion-number #(2 1)))))
