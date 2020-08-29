(in-package :cl-user)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (sb-int:defconstant-eqx opt
    #+swank '(optimize (speed 3) (safety 2))
    #-swank '(optimize (speed 3) (safety 0) (debug 0))
    #'equal)
  #+swank (ql:quickload '(:cl-debug-print :fiveam) :silent t)
  #-swank (set-dispatch-macro-character
           #\# #\> (lambda (s c p) (declare (ignore c p)) `(values ,(read s nil nil t)))))
#+swank (cl-syntax:use-syntax cl-debug-print:debug-print-syntax)

(macrolet ((def-int (b)
             `(progn (deftype ,(intern (format nil "UINT~A" b)) () '(unsigned-byte ,b))
                     (deftype ,(intern (format nil "INT~A" b)) () '(signed-byte ,b))))
           (defs (&rest bits) `(progn ,@(mapcar (lambda (b) `(def-int ,b)) bits))))
  (defs 2 4 7 8 15 16 31 32 62 63 64))

(defconstant +mod+ 1000000007)

(defmacro dbg (&rest forms)
  #+swank (if (= (length forms) 1)
              `(format *error-output* "~A => ~A~%" ',(car forms) ,(car forms))
              `(format *error-output* "~A => ~A~%" ',forms `(,,@forms)))
  #-swank (declare (ignore forms)))

(declaim (inline println))
(defun println (obj &optional (stream *standard-output*))
  (let ((*read-default-float-format* 'double-float))
    (prog1 (princ obj stream) (terpri stream))))

;; BEGIN_INSERTED_CONTENTS
;; BEGIN_USE_PACKAGE
(in-package :cl-user)

;;;
;;; Body
;;;

(defun main ()
  (let* ()))

#-swank (main)

;;;
;;; Test and benchmark
;;;

#+swank
(defun get-clipbrd ()
  (with-output-to-string (out)
    #+os-windows (run-program "powershell.exe" '("-Command" "Get-Clipboard") :output out :search t)
    #+os-unix (run-program "xsel" '("-b" "-o") :output out :search t)))

#+swank (defparameter *this-pathname* (uiop:current-lisp-file-pathname))
#+swank (defparameter *dat-pathname* (uiop:merge-pathnames* "test.dat" *this-pathname*))

#+swank
(defun run (&optional thing (out *standard-output*))
  "THING := null | string | symbol | pathname

null: run #'MAIN using the text on clipboard as input.
string: run #'MAIN using the string as input.
symbol: alias of FIVEAM:RUN!.
pathname: run #'MAIN using the text file as input."
  (let* ((*standard-output* (or out (make-string-output-stream)))
         (res (etypecase thing
                (null
                 (with-input-from-string (*standard-input* (delete #\Return (get-clipbrd)))
                   (main)))
                (string
                 (with-input-from-string (*standard-input* (delete #\Return thing))
                   (main)))
                (symbol (5am:run! thing))
                (pathname
                 (with-open-file (*standard-input* thing)
                   (main))))))
    (if out res (get-output-stream-string *standard-output*))))

#+swank
(defun gen-dat ()
  (uiop:with-output-file (out *dat-pathname* :if-exists :supersede)
    (format out "")))

#+swank
(defun bench (&optional (out (make-broadcast-stream)))
  (time (run *dat-pathname* out)))

#-swank
(eval-when (:compile-toplevel)
  (when (and (boundp 'sb-c::*compiler-warning-count*)
             (> sb-c::*compiler-warning-count* 0))
    (sb-ext:quit :unix-status 1)))
