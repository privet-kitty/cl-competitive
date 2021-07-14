(defpackage :cp/crt
  (:use :cl :cp/ext-gcd)
  (:export #:crt #:crt*)
  (:documentation "Provides bignum arithmetic by Chinese remainder theorem.

Reference:
https://qiita.com/drken/items/ae02240cd1f8edfc86fd (Japanese)"))
(in-package :cp/crt)

(declaim (inline crt))
(defun crt (b1 mod1 b2 mod2)
  "Solves x ≡ b1 mod m1, x ≡ b2 mod m2. The returned integer is in [0, LCM(m1,
m2)). Returns LCM(m1, m2) as the second value.

- This function returns (VALUES NIL NIL) when the system is infeasible.
- If what you need is a positive solution, just adopt LCM instead of zero."
  (declare (integer b1 b2)
           ((integer 1) mod1 mod2))
  (multiple-value-bind (p q) (ext-gcd mod1 mod2)
    (let ((gcd (+ (* p mod1) (* q mod2))))
      (declare (unsigned-byte gcd))
      (unless (zerop (mod (- b2 b1) gcd))
        ;; b1 ≡ b2 mod gcd(m1, m2) must hold
        (return-from crt (values nil nil)))
      (let* ((lcm/mod1 (floor mod2 gcd))
             (tmp (mod (* (floor (- b2 b1) gcd) p) lcm/mod1))
             (lcm (* mod1 lcm/mod1)))
        (values (mod (+ b1 (* mod1 tmp)) lcm) lcm)))))

(defun crt* (rems moduli)
  "Solves x_i ≡ b_i mod m_i, for i in {1, 2, ..., k}. The returned integers are
in [0, LCM(m_1, m_2, ..., m_k)). Returns LCM(m_1, m_2, ..., m_k} as the second
value.

- This function returns (VALUES NIL NIL) when the system is infeasible.
- If what you need is a positive solution, just adopt LCM instead of zero.

REMS := vector of integers
MODULI := vector of positive integers"
  (declare (vector rems moduli))
  (let ((result 0)
        (modulus 1))
    (declare (unsigned-byte result modulus))
    (dotimes (i (length rems))
      ;; Iteratively solves the system of two equations: x1 ≡ b1 mod m1 and x2
      ;; ≡ b2 mod m2, where RESULT = b1, MODULUS = m1, (AREF REMS I) = b2, and
      ;; (AREF MODULI I) = m2.
      (let ((b2 (aref rems i))
            (m2 (aref moduli i)))
        (declare (integer b2)
                 ((integer 1) m2))
        (multiple-value-bind (p q) (ext-gcd modulus m2)
          (let ((gcd (+ (* p modulus) (* q m2))))
            (declare (unsigned-byte gcd))
            (unless (zerop (mod (- b2 result) gcd))
              ;; b1 ≡ b2 mod gcd(m1, m2) must holds
              (return-from crt* (values nil nil)))
            (let* ((lcm/m1 (floor m2 gcd))
                   (tmp (mod (* (floor (- b2 result) gcd) p) lcm/m1)))
              (declare (unsigned-byte lcm/m1 tmp))
              (setq result (+ result (* modulus tmp)))
              (setq modulus (* modulus lcm/m1)))))))
    (values result modulus)))
