;; -*- coding: utf-8 -*-
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter OPT
    #+swank '(optimize (speed 3) (safety 2))
    #-swank '(optimize (speed 3) (safety 0) (debug 0)))
  #+swank (ql:quickload '(:cl-debug-print :fiveam))
  #-swank (set-dispatch-macro-character #\# #\> (lambda (s c p) (declare (ignore c p)) (read s nil (values) t))))
#+swank (cl-syntax:use-syntax cl-debug-print:debug-print-syntax)
#-swank (disable-debugger) ; for CS Academy

;; BEGIN_INSERTED_CONTENTS
(defmacro dbg (&rest forms)
  #+swank
  (if (= (length forms) 1)
      `(format *error-output* "~A => ~A~%" ',(car forms) ,(car forms))
      `(format *error-output* "~A => ~A~%" ',forms `(,,@forms)))
  #-swank (declare (ignore forms)))

(defmacro define-int-types (&rest bits)
  `(progn
     ,@(mapcar (lambda (b) `(deftype ,(intern (format nil "UINT~A" b)) () '(unsigned-byte ,b))) bits)
     ,@(mapcar (lambda (b) `(deftype ,(intern (format nil "INT~A" b)) () '(signed-byte ,b))) bits)))
(define-int-types 2 4 7 8 15 16 31 32 62 63 64)

(declaim (inline println))
(defun println (obj &optional (stream *standard-output*))
  (let ((*read-default-float-format* 'double-float))
    (prog1 (princ obj stream) (terpri stream))))

(defconstant +mod+ 1000000007)

;; Body

(defun main ()
  (let* ()))

#-swank (main)


;; For Test
#+swank
(defun io-equal (in-string out-string &optional (func #'main))
  "Passes IN-STRING to *STANDARD-INPUT*, executes FUNC, and returns true if the
string output to *STANDARD-OUTPUT* is equal to OUT-STRING."
  (labels ((ensure-last-lf (s)
             (if (and (> (length s) 0)
                      (eql (char s (- (length s) 1)) #\Linefeed))
                 s
                 (uiop:strcat s uiop:+lf+))))
    (equal (ensure-last-lf out-string)
           (with-output-to-string (out)
             (let ((*standard-output* out))
               (with-input-from-string (*standard-input* (ensure-last-lf in-string))
                 (funcall func)))))))

#+swank
(defun get-clipbrd ()
  (with-output-to-string (out)
    (run-program "C:/msys64/usr/bin/cat.exe" '("/dev/clipboard") :output out)))

#+swank (defparameter *this-pathname* (uiop:current-lisp-file-pathname))
#+swank (defparameter *dat-pathname* (uiop:merge-pathnames* "test.dat" *this-pathname*))

#+swank
(defun run (&optional thing (out *standard-output*))
  (let ((*standard-output* out))
    (etypecase thing
      (null ; Runs #'MAIN with the string on clipboard
       (with-input-from-string (*standard-input* (delete #\Return (get-clipbrd)))
         (main)))
      (string
       (with-input-from-string (*standard-input* (delete #\Return thing))
         (main)))
      (symbol (5am:run! thing))
      (pathname ; Runs #'MAIN with the string in a text file
       (with-open-file (*standard-input* thing)
         (main))))))

#+swank
(defun gen-dat ()
  (uiop:with-output-file (out *dat-pathname* :if-exists :supersede)
    (format out "")))

#+swank
(defun bench (&optional (out (make-broadcast-stream)))
  (time (run *dat-pathname* out)))
