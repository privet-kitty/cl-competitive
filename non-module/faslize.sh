#!/bin/bash

IMPL="sbcl-bin/2.0.6" # Default is for CS Academy. TODO: use env var?
IS_SHORT=0

USAGE="Usage: $0 [-s] [-L impl/version] lisp_file
Options:
-s : omit the original source
-L impl/version : pass impl/version to ros -L
"
usage_exit() {
    echo "${USAGE}" 1>&2
    exit 1
}

while getopts sL: OPT; do
    case $OPT in
	s) IS_SHORT=1
	   ;;
	L) IMPL=$OPTARG
	   ;;
	\?) usage_exit
	    ;;
    esac
done

shift $((OPTIND - 1))

if [ $# -ne 1 ]; then
    usage_exit
fi

LISP_PATHNAME="$1"
BASENAME=$(echo ${LISP_PATHNAME} | sed 's/\.[^\.]*$//')
FASL_PATHNAME="${BASENAME}.fasl"

ros run -L ${IMPL} -- --no-userinit --noprint --eval "(compile-file \"${LISP_PATHNAME}\" :verbose nil :print nil)" --quit
BASE64=$(base64 ${FASL_PATHNAME})

if [ $IS_SHORT -eq 0 ]; then
    HEADER_COMMENT="Source code is attached to the bottom."
else
    HEADER_COMMENT=""
fi

template=";; This file is compiled with ${IMPL}. ${HEADER_COMMENT}
(defpackage :faslize (:use :cl) (:export #:*fasl-pathname*))
(in-package :faslize)
(defparameter *fasl64* \"${BASE64}\")
(defparameter *fasl-pathname* (make-pathname :defaults *load-pathname* :type \"fasl\"))
(with-open-file (out *fasl-pathname* :direction :output
                                     :if-exists :supersede
                                     :element-type '(unsigned-byte 8))
  (sb-ext:run-program \"base64\" '(\"-id\" \"-\")
                      :search t
                      :input (make-string-input-stream *fasl64*)
                      :output out))
(in-package :cl-user)
(handler-bind ((sb-fasl::invalid-fasl-version
                 (lambda (c) (invoke-restart (find-restart 'continue c)))))
  (load faslize:*fasl-pathname*))
(sb-ext:quit)
"

echo "${template}"

if [ ${IS_SHORT} -eq 0 ]; then
    echo ";;;"
    echo ";;; Original source"
    echo ";;;"
    echo ""
    echo "#|"
    cat "${LISP_PATHNAME}"
    echo "|#"
fi
