;;;
;;; Arithmetic operations on prime factorization expression of integer
;;;

(defpackage :cp/pfactors
  (:use :cl)
  (:export #:pfactors+ #:pfactors- #:pfactors*))
(in-package :cp/pfactors)

;; TODO: document

(declaim (inline %car))
(defun %car (x)
  (the fixnum (car x)))
(declaim (inline %cdr))
(defun %cdr (x)
  (the fixnum (cdr x)))

(declaim (inline pfactors+))
(defun pfactors+ (pfactors1 pfactors2)
  (loop
    while (or pfactors1 pfactors2)
    collect (cond ((null pfactors1) (pop pfactors2))
                  ((null pfactors2) (pop pfactors1))
                  (t (let ((node1 (car pfactors1))
                           (node2 (car pfactors2)))
                       (cond ((< (%car node1) (%car node2))
                              (pop pfactors1)
                              node1)
                             ((> (%car node1) (%car node2))
                              (pop pfactors2)
                              node2)
                             (t (pop pfactors1)
                                (pop pfactors2)
                                (cons (car node1)
                                      (the fixnum
                                           (+ (%cdr node1) (%cdr node2)))))))))))

(declaim (inline pfactors-))
(defun pfactors- (pfactors1 pfactors2)
  (loop
    while (or pfactors1 pfactors2)
    collect (cond ((null pfactors1)
                   (let ((node (pop pfactors2)))
                     (cons (%car node) (the fixnum (- (%cdr node))))))
                  ((null pfactors2)
                   (pop pfactors1))
                  (t (let ((node1 (car pfactors1))
                           (node2 (car pfactors2)))
                       (cond ((< (%car node1) (%car node2))
                              (pop pfactors1)
                              node1)
                             ((> (%car node1) (%car node2))
                              (pop pfactors2)
                              (cons (%car node2) (the fixnum (- (%cdr node2)))))
                             (t (pop pfactors1)
                                (pop pfactors2)
                                (cons (car node1)
                                      (the fixnum (- (%cdr node1) (%cdr node2)))))))))))

(declaim (inline pfactors*))
(defun pfactors* (pfactors scalar)
  (declare (fixnum scalar))
  (if (zerop scalar)
      nil
      (loop for node in pfactors
            collect (cons (car node) (the fixnum (* scalar (%cdr node)))))))
