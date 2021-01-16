(defpackage :cp/test/implicit-treap
  (:use :cl :fiveam :cp/implicit-treap)
  (:import-from :cp/test/base #:base-suite)
  (:import-from :cp/implicit-treap
                #:%itreap-value #:%itreap-priority #:%itreap-left #:%itreap-right
                #:%itreap-count #:%itreap-accumulator #:%itreap-lazy #:%itreap-reversed
                #:%make-itreap #:force-down #:force-up))
(in-package :cp/test/implicit-treap)
(in-suite base-suite)

;; (declaim (ftype (function * (values (integer 0 #.most-positive-fixnum) &optional))
;;                 itreap-fold-bisect)
;;          (inline itreap-fold-bisect))
;; (defun itreap-fold-bisect (itreap test &optional (start 0))
;;   "Returns the largest index that satisfies (FUNCALL TEST (OP ITREAP[START]
;; ITREAP[START+1] ... ITREAP[index-1])).

;; Note:
;; - (FUNCALL TEST +OP-IDENTITY+) must be true.
;; - TEST must be monotone in the target range.
;; "
;;   (declare ((integer 0 #.most-positive-fixnum) start))
;;   (assert (funcall test +op-identity+))
;;   (let ((result 0))
;;     (labels
;;         ((main-search (itreap offset)
;;            (unless itreap
;;              (return-from main-search +op-identity+))
;;            (force-down itreap)
;;            (let ((sum +op-identity+)
;;                  (lcount (+ offset (itreap-count (%itreap-left itreap)))))
;;              (if (<= start lcount)
;;                  (progn
;;                    (setq sum (op sum (main-search (%itreap-left itreap) offset)))
;;                    (setq sum (op sum (%itreap-value itreap)))
;;                    (unless (funcall test sum)
;;                      (setq result lcount))
;;                    (search-subtree (%itreap-right itreap) (+ lcount 1) sum))
;;                  (main-search (%itreap-right itreap) (+ lcount 1))))
;;            (force-up itreap))
;;          (search-subtree (itreap offset prev-sum)
;;            (declare ((integer 0 #.most-positive-fixnum) offset))
;;            (unless itreap
;;              (setq result offset)
;;              (return-from search-subtree))
;;            (force-down itreap)
;;            (let ((sum prev-sum))
;;              (cond ((not (funcall test (setq sum (op sum (itreap-accumulator (%itreap-left itreap))))))
;;                     (search-subtree (%itreap-left itreap) offset prev-sum))
;;                    ((not (funcall test (setq sum (op sum (%itreap-value itreap)))))
;;                     (setq result (+ offset (itreap-count (%itreap-left itreap)))))
;;                    (t
;;                     (search-subtree (%itreap-right itreap)
;;                                     (+ offset (itreap-count (%itreap-left itreap)) 1)
;;                                     sum)))
;;              (force-up itreap))))
;;       (if (zerop start)
;;           (search-subtree itreap 0 +op-identity+)
;;           (main-search itreap 0))
;;       result)))

(defun copy-itreap (itreap)
  "For development. Recursively copies the whole ITREAPs."
  (if (null itreap)
      nil
      (%make-itreap (%itreap-value itreap)
                    (%itreap-priority itreap)
                    :left (copy-itreap (%itreap-left itreap))
                    :right (copy-itreap (%itreap-right itreap))
                    :count (%itreap-count itreap)
                    :accumulator (%itreap-accumulator itreap)
                    :lazy (%itreap-lazy itreap)
                    :reversed (%itreap-reversed itreap))))

(defun itreap-sane-p (itreap)
  (labels ((priority* (itreap)
             (if (null itreap) 0 (%itreap-priority itreap))))
    (declare (inline priority*))
    (if (null itreap)
        t
        (and (> (%itreap-priority itreap) (priority* (%itreap-left itreap)))
             (> (%itreap-priority itreap) (priority* (%itreap-right itreap)))
             (itreap-sane-p (%itreap-left itreap))
             (itreap-sane-p (%itreap-right itreap))))))

(defun itreap-list (itreap)
  (let (res)
    (labels ((recur (itreap)
               (when itreap
                 (force-down itreap)
                 (recur (%itreap-left itreap))
                 (push (%itreap-value itreap) res)
                 (recur (%itreap-right itreap))
                 (force-up itreap))))
      (recur itreap)
      (reverse res))))

(defun itreap-tree (itreap)
  (if (null itreap)
      nil
      (cons (%itreap-value itreap)
            (cons (itreap-tree (%itreap-left itreap))
                  (itreap-tree (%itreap-right itreap))))))


(test implicit-treap/sanity
  (is-true (loop for i below 100 always (itreap-sane-p (make-itreap i)))))

(test implicit-treap/insert
  (declare (notinline itreap-insert))
  (is (equal '(10 1 2 3 4 5 6)
             (itreap-list (itreap-insert (itreap 1 2 3 4 5 6) 0 10))))
  (is (equal '(1 10 2 3 4 5 6)
             (itreap-list (itreap-insert (itreap 1 2 3 4 5 6) 1 10))))
  (is (equal '(1 2 10 3 4 5 6)
             (itreap-list (itreap-insert (itreap 1 2 3 4 5 6) 2 10))))
  (is (equal '(1 2 3 10 4 5 6)
             (itreap-list (itreap-insert (itreap 1 2 3 4 5 6) 3 10))))
  (is (equal '(1 2 3 4 10 5 6)
             (itreap-list (itreap-insert (itreap 1 2 3 4 5 6) 4 10))))
  (is (equal '(1 2 3 4 5 10 6)
             (itreap-list (itreap-insert (itreap 1 2 3 4 5 6) 5 10))))
  (is (equal '(1 2 3 4 5 6 10)
             (itreap-list (itreap-insert (itreap 1 2 3 4 5 6) 6 10))))
  (is (= 10 (itreap-fold (itreap-insert (itreap 1 2 3 4 5 6) 3 10) 3 4)))
  (signals invalid-itreap-index-error
    (itreap-list (itreap-insert (itreap 1 2 3 4 5 6) 7 10)))
  (is (equal '(10) (itreap-list (itreap-insert nil 0 10)))))

(test implicit-treap/delete
  (declare (notinline itreap-delete itreap-fold))
  (is (equal '(2 3 4 5 6)
             (itreap-list (itreap-delete (itreap 1 2 3 4 5 6) 0))))
  (is (equal '(1 3 4 5 6)
             (itreap-list (itreap-delete (itreap 1 2 3 4 5 6) 1))))
  (is (equal '(1 2 4 5 6)
             (itreap-list (itreap-delete (itreap 1 2 3 4 5 6) 2))))
  (is (equal '(1 2 3 5 6)
             (itreap-list (itreap-delete (itreap 1 2 3 4 5 6) 3))))
  (is (equal '(1 2 3 4 6)
             (itreap-list (itreap-delete (itreap 1 2 3 4 5 6) 4))))
  (is (equal '(1 2 3 4 5)
             (itreap-list (itreap-delete (itreap 1 2 3 4 5 6) 5))))
  (signals invalid-itreap-index-error (itreap-delete (itreap 1 2 3 4 5 6) 6))
  (signals invalid-itreap-index-error (itreap-delete nil 0))
  (let ((itreap (itreap 1 2 3 2 1 2 7)))
    (is (= 1 (itreap-fold itreap 1 5)))
    (setq itreap (itreap-delete itreap 4))
    (is (= 2 (itreap-fold itreap 1 5)))))

(test implicit-treap
  (dotimes (i 10)
    (let ((itreap1 (itreap 1 2 3 2 1 2 7)))
      (is (equal '(1 2 3 2 1 2 7) (itreap-list itreap1)))
      ;; ref and fold
      (is (= 7 (itreap-ref itreap1 6)))
      (is (= 1 (itreap-fold itreap1 0 7)))
      (is (= 2 (itreap-fold itreap1 5 7)))
      (is (= 2 (itreap-fold itreap1 1 3)))
      ;; point update
      (setf (itreap-ref itreap1 1) 0)
      (is (= 0 (itreap-ref itreap1 1)))
      (is (= most-positive-fixnum (itreap-fold itreap1 1 1)))
      (is (= 0 (itreap-fold itreap1 0 3)))
      (is (= 0 (itreap-fold itreap1 0 7)))
      (is (= 1 (itreap-fold itreap1 2 7)))
      (is (= 2 (itreap-fold itreap1 2 4)))
      ;; invalid index error
      (signals invalid-itreap-index-error (itreap-fold itreap1 2 8))
      (signals invalid-itreap-index-error (itreap-fold itreap1 2 1))
      (signals invalid-itreap-index-error (itreap-ref itreap1 8))
      (signals invalid-itreap-index-error (setf (itreap-ref itreap1 8) 5))
      ;; range update
      (setf itreap1 (itreap 1 2 3 2 1 2 7))
      (setf itreap1 (itreap-update itreap1 -10 3 5))
      (is (equal '(1 2 3 -8 -9 2 7) (itreap-list itreap1)))
      (is (= -8 (itreap-fold itreap1 2 4)))
      (is (= 3 (itreap-fold itreap1 2 3)))
      ;; range update (for null range)
      (setf itreap1 (itreap-update itreap1 -10 3 3))
      (is (equal '(1 2 3 -8 -9 2 7) (itreap-list itreap1)))
      (is (= -8 (itreap-fold itreap1 2 4)))
      (is (= 3 (itreap-fold itreap1 2 3))))))

(defun %make (list)
  (let ((len (length list))
        nodes
        itreap)
    (dotimes (i len)
      (let* ((idx (random (- len i)))
             (elm (nth idx list)))
        (push (cons elm idx) nodes)
        (setq list
              (append (subseq list 0 idx)
                      (subseq list (+ idx 1))))))
    (loop for (elm . idx) in nodes
          do (itreap-push elm itreap idx))
    itreap))

(test implicit-treap-fold-bisect
  (declare (notinline itreap-fold-bisect))
  (let ((list '(5 2 4 2 1 4 2 1 -1)))
    (is (= 0 (itreap-fold-bisect (%make list) (lambda (x) (> x 6)))))
    (is (= 0 (itreap-fold-bisect (%make list) (lambda (x) (> x 5)))))
    (is (= 1 (itreap-fold-bisect (%make list) (lambda (x) (> x 4)))))
    (is (= 1 (itreap-fold-bisect (%make list) (lambda (x) (> x 3)))))
    (is (= 1 (itreap-fold-bisect (%make list) (lambda (x) (> x 2)))))
    (is (= 4 (itreap-fold-bisect (%make list) (lambda (x) (> x 1)))))
    (is (= 8 (itreap-fold-bisect (%make list) (lambda (x) (> x 0)))))
    (is (= 8 (itreap-fold-bisect (%make list) (lambda (x) (> x -1)))))
    (is (= 9 (itreap-fold-bisect (%make list) (lambda (x) (> x -2)))))
    
    ;; START arg
    (is (= 3 (itreap-fold-bisect (%make list) (lambda (x) (> x 6)) 3)))
    (is (= 3 (itreap-fold-bisect (%make list) (lambda (x) (> x 5)) 3)))
    (is (= 3 (itreap-fold-bisect (%make list) (lambda (x) (> x 4)) 3)))
    (is (= 3 (itreap-fold-bisect (%make list) (lambda (x) (> x 3)) 3)))
    (is (= 3 (itreap-fold-bisect (%make list) (lambda (x) (> x 2)) 3)))
    (is (= 4 (itreap-fold-bisect (%make list) (lambda (x) (> x 1)) 3)))
    (is (= 8 (itreap-fold-bisect (%make list) (lambda (x) (> x 0)) 3)))
    (is (= 8 (itreap-fold-bisect (%make list) (lambda (x) (> x -1)) 3)))
    (is (= 9 (itreap-fold-bisect (%make list) (lambda (x) (> x -2)) 3)))

    (is (= 8 (itreap-fold-bisect (%make list) (lambda (x) (> x 6)) 8)))
    (is (= 8 (itreap-fold-bisect (%make list) (lambda (x) (> x -1)) 8)))
    (is (= 9 (itreap-fold-bisect (%make list) (lambda (x) (> x -2)) 8)))

    (is (= 9 (itreap-fold-bisect (%make list) (lambda (x) (> x -10)) 9)))
    (is (= 9 (itreap-fold-bisect (%make list) (lambda (x) (> x 10)) 9)))

    (signals invalid-itreap-index-error
      (itreap-fold-bisect (%make list) (lambda (x) (> x 10)) 10))

    ;; null case
    (is (zerop (itreap-fold-bisect nil (lambda (x) (> x 10)))))))

(test implicit-treap-fold-bisect-from-end
  (declare (notinline itreap-fold-bisect-from-end))
  (let ((list '(5 -3 -3 2 -1 4 2 1 3)))
    (is (= 9 (itreap-fold-bisect-from-end (%make list) (lambda (x) (> x 6)))))
    (is (= 9 (itreap-fold-bisect-from-end (%make list) (lambda (x) (> x 5)))))
    (is (= 9 (itreap-fold-bisect-from-end (%make list) (lambda (x) (> x 4)))))
    (is (= 9 (itreap-fold-bisect-from-end (%make list) (lambda (x) (> x 3)))))
    (is (= 8 (itreap-fold-bisect-from-end (%make list) (lambda (x) (> x 2)))))
    (is (= 8 (itreap-fold-bisect-from-end (%make list) (lambda (x) (> x 1)))))
    (is (= 5 (itreap-fold-bisect-from-end (%make list) (lambda (x) (> x 0)))))
    (is (= 5 (itreap-fold-bisect-from-end (%make list) (lambda (x) (> x -1)))))
    (is (= 3 (itreap-fold-bisect-from-end (%make list) (lambda (x) (> x -2)))))
    (is (= 3 (itreap-fold-bisect-from-end (%make list) (lambda (x) (> x -3)))))
    (is (= 0 (itreap-fold-bisect-from-end (%make list) (lambda (x) (> x -4)))))
    
    ;; END arg
    (is (= 6 (itreap-fold-bisect-from-end (%make list) (lambda (x) (> x 6)) 6)))
    (is (= 6 (itreap-fold-bisect-from-end (%make list) (lambda (x) (> x 5)) 6)))
    (is (= 6 (itreap-fold-bisect-from-end (%make list) (lambda (x) (> x 4)) 6)))
    (is (= 5 (itreap-fold-bisect-from-end (%make list) (lambda (x) (> x 3)) 6)))
    (is (= 5 (itreap-fold-bisect-from-end (%make list) (lambda (x) (> x 2)) 6)))
    (is (= 5 (itreap-fold-bisect-from-end (%make list) (lambda (x) (> x 1)) 6)))
    (is (= 5 (itreap-fold-bisect-from-end (%make list) (lambda (x) (> x 0)) 6)))
    (is (= 5 (itreap-fold-bisect-from-end (%make list) (lambda (x) (> x -1)) 6)))
    (is (= 3 (itreap-fold-bisect-from-end (%make list) (lambda (x) (> x -2)) 6)))
    (is (= 3 (itreap-fold-bisect-from-end (%make list) (lambda (x) (> x -3)) 6)))
    (is (= 0 (itreap-fold-bisect-from-end (%make list) (lambda (x) (> x -4)) 6)))

    (is (= 4 (itreap-fold-bisect-from-end (%make list) (lambda (x) (> x 2)) 4)))
    (is (= 2 (itreap-fold-bisect-from-end (%make list) (lambda (x) (> x -3)) 2)))
    (is (= 0 (itreap-fold-bisect-from-end (%make list) (lambda (x) (> x -4)) 2)))

    (is (= 0 (itreap-fold-bisect-from-end (%make list) (lambda (x) (> x 10)) 0)))

    (signals invalid-itreap-index-error
      (itreap-fold-bisect-from-end (%make list) (lambda (x) (> x 10)) 10))

    ;; null case
    (is (zerop (itreap-fold-bisect-from-end nil (lambda (x) (> x 10)))))))

(test implicit-treap/sorted
  (declare (notinline itreap-bisect-left itreap-bisect-right))
  (let ((list '(3 3 2 2 2 2 1 1 1)))
    (is (= 6 (itreap-bisect-left (%make list) 1 #'>)))
    (is (= 9 (itreap-bisect-left (%make list) 0 #'>)))
    (is (= 0 (itreap-bisect-left (%make list) 4 #'>)))
    (is (= 0 (itreap-bisect-right (%make list) 4 #'>)))
    (is (= 2 (itreap-bisect-right (%make list) 3 #'>)))
    (is (= 9 (itreap-bisect-right (%make list) 1 #'>)))))

(test implicit-treap/reverse
  (declare (notinline itreap-reverse))
  (is (null (itreap-reverse nil 0 0))))
