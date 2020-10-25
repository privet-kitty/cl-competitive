;;;
;;; n-dimensional cumulative sum
;;;

(defpackage :cp/cumulative-sum
  (:use :cl)
  (:export #:define-cumulative-sum #:2dcumul-get #:2dcumul-build! #:2dcumul-update!))
(in-package :cp/cumulative-sum)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun %classify-vars (main-vars sub-vars)
    (let (plus-vars-list minus-vars-list)
      (labels ((recur (sub-vars main-vars parity vars)
                 (if (null sub-vars)
                     (if (zerop parity)
                         (push (reverse vars) plus-vars-list)
                         (push (reverse vars) minus-vars-list))
                     (progn (recur (cdr sub-vars) (cdr main-vars)
                                   (logxor 1 parity)
                                   (cons (car sub-vars) vars))
                            (recur (cdr sub-vars) (cdr main-vars)
                                   parity
                                   (cons (car main-vars) vars))))))
        (recur sub-vars main-vars 0 nil)
        (values plus-vars-list minus-vars-list))))

  (defun %rotate (list offset)
    (append (subseq list offset) (subseq list 0 offset)))

  (defun %make-builder-form (array vars dims dims+1 offset inc-macro)
    (let* ((rank (length vars))
           (rot-vars (%rotate vars offset))
           (rot-dims (%rotate dims offset))
           (rot-dims+1 (%rotate dims+1 offset))
           (target-pos (mod (- offset 1) rank))
           (src-vars vars)
           (dest-vars (loop for i below rank
                            for var in vars
                            collect (if (= i target-pos) `(+ ,var 1) var))))
      (labels ((recur (vars dims dims+1)
                 (cond ((null vars)
                        `(,inc-macro (aref ,array ,@dest-vars) (aref ,array ,@src-vars)))
                       ((null (cdr vars))
                        `(dotimes (,(car vars) ,(car dims))
                           ,(recur (cdr vars) (cdr dims) (cdr dims+1))))
                       (t
                        `(dotimes (,(car vars) ,(car dims+1))
                           ,(recur (cdr vars) (cdr dims) (cdr dims+1)))))))
        (recur rot-vars rot-dims rot-dims+1)))))

;; TODO: usage example
(defmacro define-cumulative-sum (name rank &key (+ '+) (- '-) package)
  "Provides <RANK>-dimensional cumulative sum. This macro defines three
functions: <NAME>-BUILD!, <NAME>-GET, and <NAME>-UPDATE!.

<NAME>-BUILD! takes an array and (destructively) makes it to store cumulative
sums. <NAME>-GET takes an already built array and 2*RANK indices, and returns
the sum of a given (n-dimensional) rectangle. <NAME>-UPDATE! takes a non-built
array, a value, and 2*RANK indices. This function updates the region of a
given (n-dimensional) rectangle by the value. After the array is finalized with
<NAME>-BUILD!, you can get any rectangle sums by <NAME>-GET."
  (check-type name (or symbol string))
  (check-type rank (integer 1))
  (let* ((package (or package #+sbcl (sb-int:sane-package) #-sbcl *package*))
         (getter (intern (format nil "~A-GET" name) package))
         (builder (intern (format nil "~A-BUILD!" name) package))
         (updater (intern (format nil "~A-UPDATE!" name) package))
         (inc-macro (gensym "INCF"))
         (dec-macro (gensym "DECF"))
         ;; FIXME: awkward parameter naming
         (lo-vars (case rank
                    (1 '(l))
                    (2 '(i-lo j-lo))
                    (otherwise (loop for i below rank
                                     collect (intern (format nil "LO~A" i) package)))))
         (hi-vars (case rank
                    (1 '(r))
                    (2 '(i-hi j-hi))
                    (otherwise (loop for i below rank
                                     collect (intern (format nil "HI~A" i) package)))))
         (dims+1 (loop for i below rank collect (intern (format nil "DIM~A+1" i) package)))
         (dims (loop for i below rank collect (intern (format nil "DIM~A" i) package)))
         (vars (loop for i below rank collect (intern (format nil "I~A" i) package))))
    `(progn
       (define-modify-macro ,inc-macro (new-value) ,+)
       (define-modify-macro ,dec-macro (new-value) ,-)
       (declaim (inline ,getter))
       ,(multiple-value-bind (plus-vars-list minus-vars-list)
            (%classify-vars hi-vars lo-vars)
          `(defun ,getter (array ,@lo-vars ,@hi-vars)
             (let ((result (aref array ,@hi-vars)))
               ,@(loop for vars in plus-vars-list
                       unless (equal vars hi-vars)
                       collect `(,inc-macro result (aref array ,@vars)))
               ,@(loop for vars in minus-vars-list
                       collect `(,dec-macro result (aref array ,@vars)))
               result)))
       (declaim (inline ,updater))
       ,(multiple-value-bind (plus-vars-list minus-vars-list)
            (%classify-vars lo-vars hi-vars)
          `(defun ,updater (array delta ,@lo-vars ,@hi-vars)
             ,@(loop for vars in plus-vars-list
                     collect `(,inc-macro (aref array ,@vars) delta))
             ,@(loop for vars in minus-vars-list
                     collect `(,dec-macro (aref array ,@vars) delta))
             array))
       (declaim (inline ,builder))
       (defun ,builder (array)
         (destructuring-bind ,dims+1 (array-dimensions array)
           (declare ((mod #.array-total-size-limit) ,@dims+1))
           (let ,(loop for dim in dims
                       for dim+1 in dims+1
                       collect `(,dim (- ,dim+1 1)))
             (declare ((mod #.array-total-size-limit) ,@dims))
             ,@(loop for offset below rank
                     collect (%make-builder-form 'array vars dims dims+1 offset inc-macro))))))))

(define-cumulative-sum 2dcumul 2)
