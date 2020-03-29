;;;
;;; Disjoint set by Union-Find algorithm over arbitrary monoid
;;;

;; not tested

(defmacro define-disjoint-set (name &key (operation '#'+) (element-type 'fixnum) conc-name)
  (check-type name symbol)
  (let* ((conc-string (if conc-name
                          (symbol-name conc-name)
                          (format nil "~A-" (symbol-name name))))
         (constructor (intern (format nil "MAKE-~A" (symbol-name name))))
         (rooter (intern (format nil "~AROOT" conc-string)))
         (reffer (intern (format nil "~AREF" conc-string)))
         (uniter (intern (format nil "~AUNITE!" conc-string)))
         (connectivity-checker (intern (format nil "~ACONNECTED-P" conc-string)))
         (size-getter (intern (format nil "~ASIZE" conc-string)))
         (data-accessor (intern (format nil "~ADATA" conc-string)))
         (values-accessor (intern (format nil "~AVALUES" conc-string))))
    `(progn
       (defstruct (,name
                   (:constructor ,constructor
                       (size
                        &optional
                        (contents (make-array size :element-type ',element-type))
                        &aux
                        (values
                         (prog1 contents
                           (assert (= (length contents) size))))
                        (data (make-array size :element-type 'fixnum :initial-element -1))))
                   ,@(when conc-name `((:conc-name ,(intern conc-string)))))
         (data nil :type (simple-array fixnum (*)))
         (values nil :type (simple-array ,element-type (*))))
       
       (declaim (ftype (function * (values (mod #.array-total-size-limit) &optional))
                       ,rooter))
       (defun ,rooter (,name x)
         "Returns the root of X."
         (declare (optimize (speed 3))
                  ((mod #.array-total-size-limit) x))
         (let ((data (,data-accessor ,name)))
           (if (< (aref data x) 0)
               x
               (setf (aref data x) (,rooter ,name (aref data x))))))
       
       (declaim (inline ,reffer))
       (defun ,reffer (,name x)
         (aref (,values-accessor ,name)
               (,rooter ,name x)))
       
       (declaim (inline ,uniter))
       (defun ,uniter (,name x1 x2)
         "Destructively unites X1 and X2 and returns true iff X1 and X2 become
connected for the first time."
         (let ((root1 (,rooter ,name x1))
               (root2 (,rooter ,name x2)))
           (unless (= root1 root2)
             (let ((data (,data-accessor ,name))
                   (values (,values-accessor ,name)))
               ;; ensure the size of root1 >= the size of root2
               (when (> (aref data root1) (aref data root2))
                 (rotatef root1 root2))
               (incf (aref data root1) (aref data root2))
               (setf (aref values root1)
                     (funcall ,operation (aref values root2) (aref values root1)))
               (setf (aref data root2) root1)))))

       (declaim (inline ,connectivity-checker))
       (defun ,connectivity-checker (,name x1 x2)
         "Returns true iff X1 and X2 have the same root."
         (= (,rooter ,name x1) (,rooter ,name x2)))

       (declaim (inline ,size-getter))
       (defun ,size-getter (,name x)
         "Returns the size of the connected component to which X belongs."
         (- (aref (,data-accessor ,name)
                  (,rooter ,name x)))))))


(define-disjoint-set disjoint-set
  :operation #'+
  :element-type fixnum
  :conc-name ds-)
