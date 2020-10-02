;;;
;;; n-dimensional cumulative sum
;;;

(defpackage :cp/cumulative-sum
  (:use :cl)
  (:export #:define-cumulative-sum #:2dcumul-get #:2dcumul-build!))
(in-package :cp/cumulative-sum)

(defun %classify-vars (lo-vars hi-vars)
  (let (plus-vars-list minus-vars-list)
    (labels ((recur (lo-vars hi-vars parity vars)
               (if (null lo-vars)
                   (if (zerop parity)
                       (push (reverse vars) plus-vars-list)
                       (push (reverse vars) minus-vars-list))
                   (progn (recur (cdr lo-vars) (cdr hi-vars)
                                 (logxor 1 parity)
                                 (cons (car lo-vars) vars))
                          (recur (cdr lo-vars) (cdr hi-vars)
                                 parity
                                 (cons (car hi-vars) vars))))))
      (recur lo-vars hi-vars 0 nil)
      (values plus-vars-list minus-vars-list))))

(defun %rotate (list offset)
  (append (subseq list offset) (subseq list 0 offset)))

(defun %make-updater-form (array vars dims dims+1 offset plus)
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
                      `(setf (aref ,array ,@dest-vars)
                             (,plus (aref ,array ,@dest-vars)
                                    (aref ,array ,@src-vars))))
                     ((null (cdr vars))
                      `(dotimes (,(car vars) ,(car dims))
                         ,(recur (cdr vars) (cdr dims) (cdr dims+1))))
                     (t
                      `(dotimes (,(car vars) ,(car dims+1))
                         ,(recur (cdr vars) (cdr dims) (cdr dims+1)))))))
      (recur rot-vars rot-dims rot-dims+1))))

(defmacro define-cumulative-sum (name rank &key (+ '+) (- '-) package)
  "Provides <RANK>-dimensional cumulative sum. This function defines two
functions: <NAME>-BUILD! and <NAME>-GET. The first function receives an array
and (destructively) makes it to store cumulative sums. The second function takes
the built array and 2*RANK indices, and returns the sum of a
given (n-dimensional) rectangle."
  (let* ((package (or package #+sbcl (sb-int:sane-package) #-sbcl *package*))
         (getter (intern (format nil "~A-GET" name) package))
         (builder (intern (format nil "~A-BUILD!" name) package))
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
    (multiple-value-bind (plus-vars-list minus-vars-list) (%classify-vars lo-vars hi-vars)
      `(progn
         (declaim (inline ,getter))
         (defun ,getter (array ,@lo-vars ,@hi-vars)
           ;; FIXME: should we use funcall?
           (,- (,+ ,@(loop for vars in plus-vars-list
                           collect `(aref array ,@vars)))
               (,+ ,@(loop for vars in minus-vars-list
                           collect `(aref array ,@vars)))))
         (declaim (inline ,builder))
         (defun ,builder (array)
           (destructuring-bind ,dims+1 (array-dimensions array)
             (declare ((mod #.array-total-size-limit) ,@dims+1))
             (let ,(loop for dim in dims
                         for dim+1 in dims+1
                         collect `(,dim (- ,dim+1 1)))
               (declare ((mod #.array-total-size-limit) ,@dims))
               ,@(loop for offset below rank
                       collect (%make-updater-form 'array vars dims dims+1 offset +)))))))))

(define-cumulative-sum 2dcumul 2)
