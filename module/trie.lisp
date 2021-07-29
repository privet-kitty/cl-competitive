(defpackage :cp/trie
  (:use :cl)
  (:export #:trie-char-encode #:trie-node #:make-trie #:+null-trie+ #:+trie-alphabet-size+
           #:trie-size #:trie-children #:trie-insert! #:trie-get)
  (:documentation "Provides multiset structure with trie."))
(in-package :cp/trie)

;; ASCII code:
;; #\A: 65
;; #\a: 97
;; #\0: 48
(declaim (inline trie-char-encode))
(defun trie-char-encode (x)
  (- (char-code x) #.(char-code #\`)))

(defconstant +trie-alphabet-size+ 27)
(defconstant +null-trie+ 0)

(declaim (inline %make-trie))
(defstruct (trie (:constructor %make-trie
                     (&aux (children (make-array +trie-alphabet-size+
                                                 :element-type t
                                                 :initial-element +null-trie+))))
                 (:copier nil)
                 (:predicate nil))
  (size 0 :type (mod #.most-positive-fixnum))
  (children nil :type (simple-array t (#.+trie-alphabet-size+))))

(declaim (inline make-trie))
(defun make-trie () (%make-trie))

(declaim (inline trie-insert!))
(defun trie-insert! (trie string)
  "Inserts STRING to TRIE."
  (declare (vector string))
  (let ((end (length string)))
    (labels ((recur (node position)
               (incf (trie-size node))
               (unless (= position end)
                 (let ((children (trie-children node))
                       (char (trie-char-encode (aref string position))))
                   (when (eql +null-trie+ (aref children char))
                     (setf (aref children char) (%make-trie)))
                   (recur (aref children char) (+ 1 position))))))
      (recur trie 0)
      trie)))

(declaim (inline trie-get))
(defun trie-get (trie string &key (start 0) end (target :whole))
  "Returns the number of STRINGs which are registered in TRIE and

1. whose prefix coincide with STRING, if TARGET is :PREFIX;
2. which coincide with STRING, if TARGET is :WHOLE."
  (declare (vector string)
           ((mod #.array-dimension-limit) start)
           ((or null (mod #.array-dimension-limit)) end)
           ((member :prefix :whole) target))
  (let ((end (or end (length string))))
    (labels ((recur (node position)
               (if (= position end)
                   (let ((size (trie-size node)))
                     (when (eq target :whole)
                       (loop for child across (trie-children node)
                             unless (eql +null-trie+ child)
                             do (decf size (trie-size child))))
                     size)
                   (let ((children (trie-children node))
                         (char (trie-char-encode (aref string position))))
                     (if (eql +null-trie+ (aref children char))
                         0
                         (recur (aref children char) (+ 1 position)))))))
      (recur trie start))))
