;;;
;;; Clojure-style placehoder syntax
;;;
;;; Examples:
;;; #%(+ %1 %3) is expanded into (LAMBDA (#:G1 #:G2 #:G3) (+ #:G1 #:G3)).
;;; #%(push % stack) is expanded into (LAMBDA (#:G1) (PUSH #:G1 STACK)).
;;;

;; TODO: %&

(defun placeholder-p (symbol)
  (let ((name (symbol-name symbol)))
    (and (>= (length name) 1)
         (char= (char name 0) #\%)
         (loop for i from 1 below (length name)
               always (digit-char-p (char name i))))))

(defun get-place-number (symbol)
  (let* ((name (symbol-name symbol))
         (result (if (= 1 (length name))
                     1
                     (parse-integer name :start 1))))
    (assert (>= result 1) (result) "%0 is not allowed.")
    result))

(defun parse-placeholder-form (form)
  (let ((arity 0)
        (args (make-array 1 :fill-pointer 0)))
    (labels ((push-arg (pos)
               (loop for i from arity below pos
                     do (vector-push-extend (gensym) args))
               (setq arity (max arity pos)))
             (parse (x)
               (cond ((and (symbolp x)
                           (placeholder-p x))
                      (let ((pos (get-place-number x))) ; 1-based
                        (push-arg pos)
                        (aref args (- pos 1))))
                     ((consp x)
                      (mapcar #'parse x))
                     (t x))))
      (let ((body (parse form))
            (lambda-list (coerce args 'list)))
        `(lambda ,lambda-list
           (declare (ignorable ,@lambda-list))
           ,body)))))

(defun read-placeholder-form (s c p)
  (declare (ignore c p))
  (let ((form (read s nil nil t)))
    (parse-placeholder-form form)))

;; For CL-SYNTAX
;; (cl-syntax:defsyntax placeholder-syntax
;;   (:merge :standard)
;;   (:dispatch-macro-char #\# #\% #'read-placeholder))

;; (cl-syntax:use-syntax placeholder-syntax)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (set-dispatch-macro-character #\# #\% #'read-placeholder-form))
