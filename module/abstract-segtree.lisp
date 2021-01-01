(defpackage :cp/abstract-segtree
  (:use :cl)
  (:export #:define-segtree)
  (:documentation "Provides 1-dimensional segment tree on arbitrary
monoid (bottom-up implementation)."))
(in-package :cp/abstract-segtree)

(defmacro define-segtree (name &key (operator '#'+) (identity 0) element-type)
  "OPERATOR := binary operator (comprising a monoid)
IDENTITY := object (identity element of the monoid)
ELEMENT-TYPE := type specifier

This macro defines five functions:

- <NAME>-REF: accessor and setter
- <NAME>-FOLD: query function for range sum
- MAKE-<NAME>: linear time constructor
- <NAME>-MAX-RIGHT: binary search w.r.t. range sum in nomal order
- <NAME>-MIN-LEFT: binary search w.r.t. range sum in reverse order"
  (let* ((fname-ref (intern (format nil "~A-REF" (symbol-name name))))
         (fname-fold (intern (format nil "~A-FOLD" (symbol-name name))))
         (fname-max-right (intern (format nil "~A-MAX-RIGHT" (symbol-name name))))
         (fname-min-left (intern (format nil "~A-MIN-LEFT" (symbol-name name))))
         (fname-make (intern (format nil "MAKE-~A" (symbol-name name))))
         (fname-%make (intern (format nil "%MAKE-~A" (symbol-name name))))
         (fname-n (intern (format nil "%~A-N" (symbol-name name))))
         (fname-vector (intern (format nil "%~A-VECTOR" (symbol-name name))))
         (conc-name (intern (format nil "%~A-" (symbol-name name)))))
    `(progn
       (defstruct (,name (:constructor ,fname-%make
                             (vector &aux (n (ash (+ 1 (length vector)) -1))))
                         (:conc-name ,conc-name))
         (n nil :type (integer 0 #.(floor array-total-size-limit 2)))
         (vector nil :type (simple-array ,element-type (*))))
       (declaim (inline ,fname-make))
       (defun ,fname-make (size &key (initial-element ,identity) initial-contents)
         (declare ((mod #.array-total-size-limit) size)
                  ((or null sequence) initial-contents))
         (let ((res (make-array (max 0 (- (* 2 size) 1))
                                :element-type ',element-type
                                :initial-element initial-element)))
           (when initial-contents
             (replace res initial-contents :start1 (max 0 (- size 1))))
           (loop for i from (- size 2) downto 0
                 do (setf (aref res i)
                          (funcall ,operator
                                   (aref res (+ (* 2 i) 1)) (aref res (+ (* 2 i) 2)))))
           (,fname-%make res)))

       (declaim (inline ,fname-ref))
       (defun ,fname-ref (,name index)
         "Returns the element at INDEX."
         (declare ((integer 0 #.most-positive-fixnum) index))
         (aref (,fname-vector ,name)
               (+ index (,fname-n ,name) -1)))

       (declaim (inline (setf ,fname-ref)))
       (defun (setf ,fname-ref) (new-value ,name index)
         (declare ((mod #.array-total-size-limit) index)
                  (,element-type new-value))
         (let* ((vector (,fname-vector ,name))
                (i (+ index (- (,fname-n ,name) 1))))
           (declare ((mod #.array-total-size-limit) i))
           (setf (aref vector i) new-value)
           (loop while (> i 0)
                 do (setq i (ash (- i 1) -1))
                    (setf (aref vector i)
                          (funcall ,operator
                                   (aref vector (+ (* 2 i) 1))
                                   (aref vector (+ (* 2 i) 2)))))
           new-value))

       (declaim (ftype (function * (values ,element-type &optional)) ,fname-fold)
                (inline ,fname-fold))
       (defun ,fname-fold (,name left right)
         "Folds the given half-open range [LEFT, RIGHT)."
         (declare ((mod #.array-total-size-limit) left right))
         (let* ((vector (,fname-vector ,name))
                (l (max 0 (+ left (,fname-n ,name) -1)))
                (r (max 0 (+ right (,fname-n ,name) -1)))
                (lvalue ,identity)
                (rvalue ,identity))
           (declare ((mod #.array-total-size-limit) l r)
                    (,element-type lvalue rvalue))
           (loop while (< l r)
                 when (evenp l)
                 do (setq lvalue (funcall ,operator lvalue (aref vector l)))
                    (incf l)
                 when (evenp r)
                 do (decf r)
                    (setq rvalue (funcall ,operator (aref vector r) rvalue))
                 do (setq l (ash (- l 1) -1)
                          r (ash (- r 1) -1)))
           (funcall ,operator lvalue rvalue)))

       (declaim (ftype (function * (values (mod #.array-total-size-limit) &optional))
                       ,fname-max-right)
                (inline ,fname-max-right))
       (defun ,fname-max-right (,name test &optional (start 0))
         "Returns the rightmost index i that satisfies (FUNCALL TEST <fold of
range [START, i)>). TEST must be monotone.

Note:
- (FUNCALL TEST <identity>) must be true.
- TEST must be monotone in the target range."
         (declare ((mod #.array-total-size-limit) start))
         (assert (funcall test ,identity))
         (assert (<= start (,fname-n ,name)))
         (let* ((n (,fname-n ,name))
                (vector (,fname-vector ,name))
                (size (length vector))
                (l (max 0 (+ start (- n 1))))
                (r size)
                (value ,identity)
                (total-shift 0))
           (declare ((mod #.array-total-size-limit) l r total-shift)
                    (,element-type value))
           (labels ((recur (index)
                      (declare ((mod #.array-total-size-limit) index))
                      (loop while (< index (- n 1))
                            for new-value of-type ,element-type =
                               (funcall ,operator
                                        value
                                        (aref vector (+ 1 (* 2 index))))
                            when (funcall test new-value)
                            do (setq value new-value
                                     index (+ 2 (* 2 index)))
                            else
                            do (setq index (+ 1 (* 2 index))))
                      (return-from ,fname-max-right (- index (- n 1)))))
             (loop until (= l r)
                   when (evenp l)
                   do (let ((new-value (funcall ,operator value (aref vector l))))
                        (declare (,element-type new-value))
                        (unless (funcall test new-value)
                          (recur l))
                        (setq value new-value))
                   do (setq l (ash l -1) ; move to right block if L is even
                            r (ash (- r 1) -1)
                            total-shift (+ total-shift 1)))
             (loop for shift from (- total-shift 1) above 0
                   for r = (- (ash (+ size 1) (- shift)) 1)
                   when (evenp r)
                   do (let ((new-value (funcall ,operator value (aref vector (- r 1)))))
                        (declare (,element-type new-value))
                        (unless (funcall test new-value)
                          (recur (- r 1)))
                        (setq value new-value)))
             n)))

       (declaim (ftype (function * (values (mod #.array-total-size-limit) &optional))
                       ,fname-min-left)
                (inline ,fname-min-left))
       (defun ,fname-min-left (,name test &optional end)
         "Returns the largest index i that satisfies (FUNCALL TEST <fold of range [i,
END)>).

Note:
- (FUNCALL TEST <identity>) must be true.
- TEST must be monotone in the target range."
         (declare ((or null (mod #.array-total-size-limit)) end))
         (assert (funcall test ,identity))
         (assert (or (null end) (<= end (,fname-n ,name))))
         (let* ((n (,fname-n ,name))
                (vector (,fname-vector ,name))
                (l (max 0 (- n 1)))
                (r (max 0 (+ (or end n) (- n 1))))
                (value ,identity)
                (total-shift 0))
           (declare ((mod #.array-total-size-limit) l r total-shift)
                    (,element-type value))
           (labels ((recur (index)
                      (declare ((mod #.array-total-size-limit) index))
                      (loop while (< index (- n 1))
                            for new-value of-type ,element-type =
                               (funcall ,operator
                                        (aref vector (+ 2 (* 2 index)))
                                        value)
                            when (funcall test new-value)
                            do (setq value new-value
                                     index (+ 1 (* 2 index)))
                            else
                            do (setq index (+ 2 (* 2 index))))
                      (return-from ,fname-min-left (+ 1 (- index (- n 1))))))
             (loop until (= l r)
                   when (evenp r)
                   do (let ((new-value (funcall ,operator (aref vector (- r 1)) value)))
                        (declare (,element-type new-value))
                        (unless (funcall test new-value)
                          (recur (- r 1)))
                        (setq value new-value))
                   do (setq l (ash l -1) ; move to right block if L is even
                            r (ash (- r 1) -1)
                            total-shift (+ total-shift 1)))
             (loop for shift from (- total-shift 1) downto 0
                   for l = (ash (- n 1) (- shift))
                   when (evenp l)
                   do (let ((new-value (funcall ,operator (aref vector l) value)))
                        (declare (,element-type new-value))
                        (unless (funcall test new-value)
                          (recur l))
                        (setq value new-value)))
             0))))))

#+(or)
(define-segtree segtree
  :operator #'+
  :identity 0
  :element-type fixnum)
