(defpackage :cp/test/treap
  (:use :cl :fiveam :cp/treap)
  (:import-from :cp/test/base #:base-suite))
(in-package :cp/test/treap)
(in-suite base-suite)

(test treap
  (let ((treap nil))
    (is (null (treap-find 3 treap)))
    (is (null (treap-bisect-left treap 2)))
    (treap-push 3 treap)
    (is (= 3 (treap-find 3 treap)))
    (is (= 3 (treap-bisect-left treap 2)))
    (is (null (treap-find 5 treap)))
    (treap-push 5 treap)
    (is (= 3 (treap-bisect-left treap 3)))
    (is (= 5 (treap-bisect-left treap 4)))
    (is (= 5 (treap-bisect-left treap 5)))
    (treap-pop 3 treap)
    (is (= 5 (treap-bisect-left treap 3)))))

(test treap/push-pop
  (let ((vector (vector nil nil)))
    (is (null (treap-find 10 (aref vector 1))))
    (is (null (treap-bisect-left (aref vector 1) 15)))
    (treap-push 10 (aref vector 1))
    (treap-push 20 (aref vector 1))
    (is (null (aref vector 0)))
    (is (= 10 (treap-find 10 (aref vector 1))))
    (is (= 20 (treap-bisect-left (aref vector 1) 15)))
    (treap-pop 10 (aref vector 1))))
