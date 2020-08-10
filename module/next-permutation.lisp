;;;
;;; Next permutation (O(n))
;;; Reference:
;;; https://www.geeksforgeeks.org/find-the-next-lexicographically-greater-word-than-a-given-word/
;;;

;; NOTE: Here the underlying set is 0-based: {0, 1, 2, ..., N-1}

(define-condition no-permutation-error (error)
  ((permutation :initarg :permutation :reader no-permutation-error-permutation))
  (:report
   (lambda (condition stream)
     (format stream "~W is an extreme permutation"
             (no-permutation-error-permutation condition)))))

(defun next-permutation! (perm &key (order #'<))
  "Destructively changes PERM to the next permutation w.r.t. ORDER. ORDER must
be a strict total order on PERM."
  (declare (vector perm))
  (let* ((n (length perm))
         (left (- n 2)))
    (declare (fixnum left))
    (loop (when (< left 0)
            (error 'no-permutation-error))
          (when (funcall order (aref perm left) (aref perm (+ left 1)))
            (return))
          (decf left))
    (labels ((bisect (ok ng)
               (declare ((integer 0 #.most-positive-fixnum) ok ng))
               (if (<= (- ng ok) 1)
                   ok
                   (let ((mid (ash (+ ok ng) -1)))
                     (if (funcall order (aref perm left) (aref perm mid))
                         (bisect mid ng)
                         (bisect ok mid))))))
      (rotatef (aref perm left)
               (aref perm (bisect left n)))
      (loop for i from 1 below (ceiling (- n left) 2)
            do (rotatef (aref perm (+ left i))
                        (aref perm (- n i))))
      perm)))
