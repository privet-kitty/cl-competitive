(defpackage :cp/tree-centroid
  (:use :cl)
  (:export #:tree-centroid #:make-tree-centroid #:tc-size #:tc-graph #:tc-validities
           #:tc-find-centroid #:tc-disable-vertex #:tree-centroid-disabled-vertex-error)
  (:documentation "Provides detection of centroid of tree."))
(in-package :cp/tree-centroid)

(deftype tc-vertex-integer () '(signed-byte 32))

(defstruct (tree-centroid (:constructor make-tree-centroid
                              (graph
                               &key (vertex-key #'identity)
                               &aux
                               (size (length graph))
                               (validities (make-array size :element-type 'bit :initial-element 1))
                               (parents (make-array size :element-type 'tc-vertex-integer))
                               (subtree-sizes (make-array size :element-type 'tc-vertex-integer))))
                          (:conc-name tc-)
                          (:copier nil)
                          (:predicate nil))
  (size 0 :type (integer 0 #.most-positive-fixnum))
  (graph nil :type (simple-array list (*)))
  (validities nil :type simple-bit-vector)
  (parents nil :type (simple-array tc-vertex-integer (*)))
  (subtree-sizes nil :type (simple-array tc-vertex-integer (*)))
  (vertex-key nil :type function))

(declaim (ftype (function * (values tc-vertex-integer
                                    (or null tc-vertex-integer)
                                    &optional))
                %tc-find-centroids))
(defun %tc-find-centroids (tree-centroid root total-size)
  (declare (optimize (speed 3))
           (tc-vertex-integer total-size root))
  (let ((graph (tc-graph tree-centroid))
        (parents (tc-parents tree-centroid))
        (subtree-sizes (tc-subtree-sizes tree-centroid))
        (vertex-key (tc-vertex-key tree-centroid))
        (validities (tc-validities tree-centroid))
        centroid1
        centroid2)
    (labels ((recur (vertex parent)
               (declare (tc-vertex-integer vertex parent))
               (setf (aref parents vertex) parent)
               (let ((subtree-size 1)
                     (centroid-p t))
                 (declare (tc-vertex-integer subtree-size))
                 (dolist (edge (aref graph vertex))
                   (let ((child (funcall vertex-key edge)))
                     (declare (tc-vertex-integer child))
                     (when (and (/= child parent)
                                (= 1 (sbit validities child)))
                       (recur child vertex)
                       (when (> (aref subtree-sizes child)
                                (floor total-size 2))
                         (setq centroid-p nil))
                       (incf subtree-size (aref subtree-sizes child)))))
                 (when (> (- total-size subtree-size)
                          (floor total-size 2))
                   (setq centroid-p nil))
                 (setf (aref subtree-sizes vertex) subtree-size)
                 (when centroid-p
                   (if centroid1
                       (setq centroid2 vertex)
                       (setq centroid1 vertex))))))
      (recur root -1)
      (values centroid1 centroid2))))

(declaim (inline tc-disable-vertex))
(defun tc-disable-vertex (tree-centroid vertex)
  "Cuts the graph at VERTEX. (used for recursive decompositions)"
  (declare (tc-vertex-integer vertex))
  (setf (aref (tc-validities tree-centroid) vertex) 0)
  nil)

(declaim (inline tc-valid-p))
(defun tc-valid-p (tree-centroid vertex)
  "Returns true iff VERTEX is not disabled."
  (declare (tc-vertex-integer vertex))
  (= 1 (aref (tc-validities tree-centroid) vertex)))

(define-condition tree-centroid-disabled-vertex-error (error)
  ((tree-centroid :initarg :tree-centroid :accessor tree-centroid-disabled-vertex-error-tree-centroid)
   (vertex :initarg :vertex :accessor tree-centroid-disabled-vertex-error-vertex))
  (:report
   (lambda (c s)
     (format s "~W is disabled in ~W"
             (tree-centroid-disabled-vertex-error-tree-centroid c)
             (tree-centroid-disabled-vertex-error-vertex c)))))

(defun %tc-calc-component-size (tree-centroid root)
  (declare (optimize (speed 3)))
  (let ((validities (tc-validities tree-centroid))
        (graph (tc-graph tree-centroid))
        (vertex-key (tc-vertex-key tree-centroid)))
    (labels ((recur (vertex parent)
               (let ((size 1))
                 (declare (tc-vertex-integer size))
                 (dolist (edge (aref graph vertex))
                   (let ((child (funcall vertex-key edge)))
                     (declare (tc-vertex-integer child))
                     (when (and (/= child parent)
                                (= 1 (sbit validities child)))
                       (incf size (recur child vertex)))))
                 size)))
      (recur root -1))))

(defun tc-find-centroid (tree-centroid root &optional component-size)
  "Returns four values: CENTROID1, CHILDREN1, CENTROID2, CHILD2.

CENTROID1 := 1st centroid
CHILDREN1 := associative list of (<child> . <size of the child subtree>) for CENTROID1
CENTROID2 := 2nd centroid (if it exists)
CHILDREN2 := associative list (if CENTROID2 exists)

If the size of the component (to which ROOT belongs) is known, you can pass it
and bypass the extra traverse (though you shouldn't use it except in an urgent
case as it is dangerous)."
  (declare (optimize (speed 3))
           (tc-vertex-integer root)
           ((or null tc-vertex-integer) component-size))
  (when (= 0 (sbit (tc-validities tree-centroid) root))
    (error 'tree-centroid-disabled-vertex-error
           :tree-centroid tree-centroid
           :vertex root))
  (let (children1
        children2
        (component-size (or component-size (%tc-calc-component-size tree-centroid root)))
        (graph (tc-graph tree-centroid))
        (validities (tc-validities tree-centroid))
        (parents (tc-parents tree-centroid))
        (subtree-sizes (tc-subtree-sizes tree-centroid))
        (vertex-key (tc-vertex-key tree-centroid)))
    (declare (tc-vertex-integer component-size))
    (multiple-value-bind (centroid1 centroid2)
        (%tc-find-centroids tree-centroid root component-size)
      (dolist (edge (aref graph centroid1))
        (let ((child (funcall vertex-key edge)))
          (when (= 1 (sbit validities child))
            (if (= child (aref parents centroid1))
                (push (cons child (- component-size (aref subtree-sizes centroid1)))
                      children1)
                (push (cons child (aref subtree-sizes child))
                      children1)))))
      (when centroid2
        (dolist (edge (aref graph centroid2))
          (let ((child (funcall vertex-key edge)))
            (when (= 1 (sbit validities child))
              (if (= child (aref parents centroid2))
                  (push (cons child (- component-size (aref subtree-sizes centroid2)))
                        children2)
                  (push (cons child (aref subtree-sizes child))
                        children2))))))
      (values centroid1 children1 centroid2 children2))))
