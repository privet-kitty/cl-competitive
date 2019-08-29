;;;
;;; Trie
;;;

;; ASCII code:
;; #\A: 65
;; #\a: 97
;; #\0: 48
(declaim (inline trie-char-encode))
(defun trie-char-encode (x)
  (- (char-code x) #.(char-code #\a)))

(defconstant +trie-alphabet-size+ 26)

;; TODO: enable it to set VALUE to NIL by distinguishing null and unbound.
(declaim (inline %make-trie-node))
(defstruct (trie-node (:constructor %make-trie-node
                          (&optional value
                           &aux (children (make-array #.+trie-alphabet-size+
                                                      :element-type t
                                                      :initial-element 0)))))
  (value nil)
  (children nil :type (simple-array t (#.+trie-alphabet-size+))))

(declaim (inline make-trie))
(defun make-trie () (%make-trie-node))

(declaim (inline trie-add!))
(defun trie-add! (trie-node string &optional (value t))
  "Adds STRING to the trie and assigns VALUE to it. Note that null value means
the string doesn't exist in the trie: that is, (trie-add! <trie-node> <string>
nil) virtually works as a deletion of <string>."
  (declare (vector string))
  (let ((end (length string)))
    (labels ((recur (node position)
               (if (= position end)
                   (unless (trie-node-value node)
                     (setf (trie-node-value node) value))
                   (let ((children (trie-node-children node))
                         (char (trie-char-encode (aref string position))))
                     (when (eql 0 (aref children char))
                       (setf (aref children char) (%make-trie-node)))
                     (recur (aref children char) (+ 1 position))))))
      (recur trie-node 0)
      trie-node)))

(declaim (inline trie-query))
(defun trie-query (trie-node string function &key (start 0) end)
  "Calls FUNCTION for each prefix of STRING existing in TRIE-NODE. FUNCTION
takes two arguments: the end position and the assigned value."
  (declare (vector string)
           ((integer 0 #.most-positive-fixnum) start)
           ((or null (integer 0 #.most-positive-fixnum)) end)
           (function function))
  (let ((end (or end (length string))))
    (labels ((recur (node position)
               (when (trie-node-value node)
                 (funcall function position (trie-node-value node)))
               (unless (= position end)
                 (let ((children (trie-node-children node))
                       (char (trie-char-encode (aref string position))))
                   (unless (eql 0 (aref children char))
                     (recur (aref children char) (+ 1 position)))))))
      (recur trie-node start))))

(defun trie-get (trie-node string &key (start 0) end)
  "Finds STRING in TRIE-NODE and returns the assigned value if it exists,
otherwise NIL."
  (declare (vector string)
           ((integer 0 #.most-positive-fixnum) start)
           ((or null (integer 0 #.most-positive-fixnum)) end))
  (let ((end (or end (length string))))
    (labels ((recur (node position)
               (if (= position end)
                   (trie-node-value node)
                   (let ((children (trie-node-children node))
                         (char (trie-char-encode (aref string position))))
                     (unless (eql 0 (aref children char))
                       (recur (aref children char) (+ 1 position)))))))
      (recur trie-node start))))

(defun trie-query-longest (trie-node string &key (start 0) end)
  "Returns the end position and the value of the longest word in TRIE-NODE which
coincides with a prefix of STRING. Returns NIL when no such words exist."
  (declare (vector string)
           ((integer 0 #.most-positive-fixnum) start)
           ((or null (integer 0 #.most-positive-fixnum)) end))
  (let ((end (or end (length string)))
        result-position
        result-value)
    (declare ((or null (integer 0 #.most-positive-fixnum)) result-position))
    (labels ((recur (node position)
               (when (trie-node-value node)
                 (setq result-position position
                       result-value (trie-node-value node)))
               (unless (= position end)
                 (let ((children (trie-node-children node))
                       (char (trie-char-encode (aref string position))))
                   (unless (eql 0 (aref children char))
                     (recur (aref children char) (+ 1 position)))))))
      (recur trie-node start)
      (values result-position result-value))))

