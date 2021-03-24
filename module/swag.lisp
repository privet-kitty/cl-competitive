(defpackage :cp/swag
  (:use :cl)
  (:export #:swag-empty-error #:make-swag #:swag #:swag-push #:swag-pop #:swag-fold)
  (:documentation "Provides sliding window aggregation over a semigroup."))
(in-package :cp/swag)

(define-condition swag-empty-error (error)
  ((swag :initarg :swag :reader swag-empty-error-swag))
  (:report
   (lambda (condition stream)
     (format stream "Attempted to pop empty swag ~W"
             (swag-empty-error-swag condition)))))

(defstruct (swag (:constructor make-swag ())
                 (:conc-name %swag-)
                 (:copier nil)
                 (:predicate nil))
  ;; FRONT and BACK stores a stack of cons cells: (object . cumulative sum)
  (front nil :type list)
  (back nil :type list))

(declaim (inline swag-push))
(defun swag-push (swag obj operation)
  "Adds OBJ to the end of SWAG."
  (push (cons obj (if (%swag-front swag)
                      (funcall operation (cdar (%swag-front swag)) obj)
                      obj))
        (%swag-front swag)))

(declaim (inline swag-pop))
(defun swag-pop (swag operation)
  "Removes the first element of SWAG."
  (unless (%swag-back swag)
    (unless (%swag-front swag)
      (error 'swag-empty-error))
    (loop for node in (%swag-front swag)
          for value = (if (%swag-back swag)
                          (funcall operation (car node) (cdar (%swag-back swag)))
                          (car node))
          do (push (rplacd node value) (%swag-back swag)))
    (setf (%swag-front swag) nil))
  (pop (%swag-back swag)))

(declaim (inline swag-fold))
(defun swag-fold (swag operation &optional identity)
  "Folds the existing object by OPERATION, which must comprise a
semiring. Returns IDENTITY when an empty SWAG is folded."
  (if (%swag-back swag)
      (if (%swag-front swag)
          (funcall operation
                   (cdar (%swag-back swag))
                   (cdar (%swag-front swag)))
          (cdar (%swag-back swag)))
      (if (%swag-front swag)
          (cdar (%swag-front swag))
          identity)))
