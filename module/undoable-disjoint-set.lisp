(defpackage :cp/undoable-disjoint-set
  (:use :cl)
  (:export #:define-undoable-disjoint-set))
(in-package :cp/undoable-disjoint-set)

(defmacro define-undoable-disjoint-set (name &key operator (element-type 'fixnum) (identity 0) conc-name)
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
         (undoer (intern (format nil "~AUNDO!" conc-string)))
         (snapshotter (intern (format nil "~ASNAPSHOT!" conc-string)))
         (rollbacker (intern (format nil "~AROLLBACK!" conc-string)))
         (data-accessor (intern (format nil "~ADATA" conc-string)))
         (contents-accessor (intern (format nil "~ACONTENTS" conc-string)))
         (end-accessor (intern (format nil "~AEND" conc-string)))
         (stack-accessor (intern (format nil "~ASTACK" conc-string)))
         (chunk (if operator 6 4)))
    `(progn
       (defstruct
           (,name
            (:constructor ,constructor
                (size
                 &key (buffer-size 1)
                      ,@(when operator `(initial-contents))
                 &aux (data (make-array size :element-type 'fixnum :initial-element -1))
                      (stack (make-array (max ,chunk (* ,chunk buffer-size))
                                         :element-type t))
                      ,@(when operator
                          `((contents
                             (let ((tmp (or initial-contents
                                            (make-array size
                                                        :element-type ',element-type
                                                        :initial-element ,identity))))
                               (assert (= (length tmp) size))
                               tmp))))))
            ,@(when conc-name `((:conc-name ,(intern conc-string))))
            (:copier nil)
            (:predicate nil))
         (data nil :type (simple-array fixnum (*)))
         (stack nil :type (simple-array t (*)))
         (end 0 :type (mod #.array-total-size-limit))
         ,@(when operator
             `((contents nil :type (simple-array ,element-type (*))))))

       (declaim (inline ,rooter))
       (defun ,rooter (,name x)
         "Returns the root of the component to which X belongs."
         (let ((data (,data-accessor ,name)))
           (labels ((recur (x)
                      (if (< (aref data x) 0)
                          x
                          (recur (aref data x)))))
             (recur x))))

       ,@(when operator
           `((declaim (inline ,reffer))
             (defun ,reffer (,name x)
               (aref (,contents-accessor ,name)
                     (,rooter ,name x)))
             
             (declaim (inline (setf ,reffer)))
             (defun (setf ,reffer) (new-value ,name x)
               (let ((root (,rooter ,name x))
                     (data (,data-accessor ,name))
                     (contents (,contents-accessor ,name)))
                 ;; record
                 (when (= (,end-accessor ,name) (length (,stack-accessor ,name)))
                   (setf (,stack-accessor ,name)
                         (adjust-array (,stack-accessor ,name)
                                       (the (mod #.array-total-size-limit) (* (,end-accessor ,name) 2)))))
                 (let ((stack (,stack-accessor ,name))
                       (end (,end-accessor ,name)))
                   (setf (aref stack end) root
                         (aref stack (+ end 1)) root
                         (aref stack (+ end 2)) (aref data root)
                         (aref stack (+ end 3)) (aref data root)
                         (aref stack (+ end 4)) (aref contents root)
                         (aref stack (+ end 5)) (aref contents root))
                   (incf (,end-accessor ,name) ,chunk))
                 (setf (aref contents root) new-value)))))

       (declaim (inline ,uniter))
       (defun ,uniter (,name x1 x2)
         "Destructively unites X1 and X2. Returns true iff X1 and X2 become connected
for the first time."
         (let ((root1 (,rooter ,name x1))
               (root2 (,rooter ,name x2))
               (data (,data-accessor ,name))
               ,@(when operator
                   `((contents (,contents-accessor ,name)))))
           ;; record
           (when (= (,end-accessor ,name) (length (,stack-accessor ,name)))
             (setf (,stack-accessor ,name)
                   (adjust-array (,stack-accessor ,name)
                                 (the (mod #.array-total-size-limit) (* (,end-accessor ,name) 2)))))
           (let ((stack (,stack-accessor ,name))
                 (end (,end-accessor ,name)))
             (setf (aref stack end) root1
                   (aref stack (+ end 1)) root2
                   (aref stack (+ end 2)) (aref data root1)
                   (aref stack (+ end 3)) (aref data root2)
                   ,@(when operator
                       `((aref stack (+ end 4)) (aref contents root1)
                         (aref stack (+ end 5)) (aref contents root2))))
             (incf (,end-accessor ,name) ,chunk))
           (unless (= root1 root2)
             ;; Ensure the size of root1 >= the size of root2
             (when (> (aref data root1) (aref data root2))
               (rotatef root1 root2))
             ;; update
             (incf (aref data root1) (aref data root2))
             ,@(when operator
                 `((setf (aref contents root1)
                         (funcall ,operator (aref contents root2) (aref contents root1)))))
             (setf (aref data root2) root1))))

       (declaim (inline ,connectivity-checker))
       (defun ,connectivity-checker (,name x1 x2)
         "Returns true iff X1 and X2 have the same root."
         (= (,rooter ,name x1) (,rooter ,name x2)))

       (declaim (inline ,size-getter))
       (defun ,size-getter (,name x)
         "Returns the size of the connected component to which X belongs."
         (- (aref (,data-accessor ,name)
                  (,rooter ,name x))))

       (defun ,undoer (,name &optional (empty-error-p t))
         "Undoes the last union or set operation."
         (symbol-macrolet ((end (,end-accessor ,name)))
           (if (zerop end)
               (and empty-error-p (error "No undoable updates in ~W" ,name))
               (let ((stack (,stack-accessor ,name))
                     (data (,data-accessor ,name))
                     ,@(when operator
                         `((contents (,contents-accessor ,name)))))
                 (setf end (- end ,chunk)
                       (aref data (aref stack end)) (aref stack (+ end 2))
                       (aref data (aref stack (+ end 1))) (aref stack (+ end 3))
                       ,@(when operator
                           `((aref contents (aref stack end)) (aref stack (+ end 4))
                             (aref contents (aref stack (+ end 1))) (aref stack (+ end 5)))))))))

       (declaim (inline ,snapshotter))
       (defun ,snapshotter (,name)
         (setf (,end-accessor ,name) 0))

       (declaim (inline ,rollbacker))
       (defun ,rollbacker (,name)
         "Rewinds ,NAME to the snapshotted point."
         (loop while (,undoer ,name nil))))))
