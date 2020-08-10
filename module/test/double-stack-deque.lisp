(defpackage :cp/test/double-stack-deque
  (:use :cl :fiveam :cp/double-stack-deque)
  (:import-from :cp/test/base #:base-suite))
(in-package :cp/test/double-stack-deque)
(in-suite base-suite)

(test double-stack-deque
  (let ((deque (make-deque '(5 6 7))))
    (is (= 7 (deque-pop-back deque)))
    (deque-push-front 4 deque)
    (deque-push-back 7 deque)
    (deque-push-back 8 deque)
    (is (= 4 (deque-pop-front deque)))
    (is (= 5 (deque-pop-front deque)))
    (is (= 6 (deque-pop-front deque)))
    (is (= 8 (deque-pop-back deque)))
    (is (= 7 (deque-pop-back deque)))
    (signals deque-empty-error (deque-pop-front deque))
    (signals deque-empty-error (deque-pop-back deque))))
