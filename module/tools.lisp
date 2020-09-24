(defpackage :cp/tools
  (:use :cl :sb-ext)
  (:export #:run #:get-clipbrd #:submit #:sub #:login))
(in-package :cp/tools)

(defun get-clipbrd ()
  (with-output-to-string (out)
    #+os-windows
    (run-program "powershell.exe" '("-Command" "Get-Clipboard") :output out :search t)
    #+os-unix
    (run-program "xsel" '("-b" "-o") :output out :search t)))

(defun run (&optional thing (out *standard-output*) main)
  "THING := null | string | symbol | pathname

null: run MAIN using the text on clipboard as input.
string: run MAIN using the string as input.
symbol: alias of FIVEAM:RUN!.
pathname: run MAIN using the text file as input.

- If MAIN is nil, #'cl-user::main is called.
- If OUT is nil, this function returns the whole output of MAIN as a string.
"
  (let* ((main (or main
                   (let ((symbol (find-symbol "MAIN" :cl-user)))
                     (if (and symbol (fboundp symbol))
                         (symbol-function symbol)
                         (error "Don't know which function to run")))))
         (*standard-output* (or out (make-string-output-stream)))
         (res (etypecase thing
                (null
                 (with-input-from-string (*standard-input* (delete #\Return (get-clipbrd)))
                   (funcall main)))
                (string
                 (with-input-from-string (*standard-input* (delete #\Return thing))
                   (funcall main)))
                (symbol (5am:run! thing))
                (pathname
                 (with-open-file (*standard-input* thing)
                   (funcall main))))))
    (if out res (get-output-stream-string *standard-output*))))

(defun submit (&key url pathname (test t))
  "If TEST is true, this function verifies the file with (RUN :SAMPLE) before
submission."
  (let ((url (or url
                 (let ((symbol (find-symbol "*URL*" :cl-user)))
                   (if (and symbol (boundp symbol))
                       (symbol-value symbol)
                       (error "Don't know to which URL to do submission")))))
        (pathname (or pathname
                      (let ((symbol (find-symbol "*THIS-PATHNAME*" :cl-user)))
                        (if (and symbol (boundp symbol))
                            (symbol-value symbol)
                            (error "Don't know which file to submit")))))
        (wait 2.5))
    (when (or (not test) (run :sample))
      (format t "Submit in ~A seconds~%" wait)
      (sleep wait)
      (run-program "oj" `("submit" "--yes" "--wait" "0" ,url ,(namestring pathname))
                   :output *standard-output*
                   :search t))))

(defun sub (&key url pathname (test t))
  "Is an alias of CP/TOOLS:SUBMIT."
  (submit :url url :pathname pathname :test test))

(defun login (&optional url)
  (let ((url (or url
                 (let ((symbol (find-symbol "*URL*" :cl-user)))
                   (if (and symbol (boundp symbol))
                       (symbol-value symbol)
                       (error "Don't know to which URL to do submission"))))))
    (run-program "oj" `("login" ,url) :output *standard-output* :search t)))
