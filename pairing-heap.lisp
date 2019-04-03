(defstruct (node (:constructor make-node (element)))
  element
  (list nil :type list))

(defun heap-merge (l r)
  (cond ((null l) r)
        ((null r) l)
        (())))
