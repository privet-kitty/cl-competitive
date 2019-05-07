;; -*- coding:utf-8 -*-

;; DEPRECATED
;; Please don't use it.

(deftype non-negative-fixnum () '(integer 0 #.most-positive-fixnum))
(defun trace-euclidean-alg (m n)
  "Gibt die Verlaufsliste des euklidischen Algorithmus zurück."
  (declare (non-negative-fixnum m n))
  (multiple-value-bind (quot rem) (floor m n)
    (if (zerop rem)
	(list quot)
	(cons (floor (- m rem) n)
	      (trace-euclidean-alg n rem)))))

(declaim (inline solve-bezout))
(defun solve-bezout (m n)
  "Gibt eine ganzzahlige Lösung von mx+ny=gcd(m, n) zurück. M und N müssen
positiv sein."
  (declare (non-negative-fixnum m n))
  (loop for k of-type non-negative-fixnum
        in (if (>= m n)
	       (trace-euclidean-alg m n)
	       (trace-euclidean-alg n m))
        for x of-type fixnum = 0 then u
        and y of-type fixnum = 1 then v
        and u of-type fixnum = 1 then (+ x (* (- k) u))
        and v of-type fixnum = (- k) then (+ y (* (- k) v))
        finally (return (if (>= m n)
                            (values x y)
                            (values y x)))))

(defun mod-inverse (a m)
  "Löst ax ≡ 1 (mod m). a und m müssen positiv und teilerfremd sein."
  (declare ((integer 0 #.most-positive-fixnum) a m))
  (mod (nth-value 0 (solve-bezout a m)) m))

(defun get-minimum-factor (x alpha)
  "Gibt k in Z zurück, so dass x+k*alpha das kleinste nicht negative Nummer
ist."
  (funcall (if (plusp alpha) #'ceiling #'floor)
	   (/ (- x) alpha)))

(defun solve-bezout-nonnegative (m n d &optional (sup most-positive-fixnum))
  "Sucht eine ganzzahlige Lösung, so dass m*x+n*y=d, 0<=x, 0<=y, x+y<=sup."
  (let ((gcd-mn (gcd m n)))
    (if (/= (mod d gcd-mn) 0)
        nil
        (multiple-value-bind (init-x init-y) (solve-bezout m n)
          (let* ((factor (floor d gcd-mn))
                 (x0 (* init-x factor))
                 (y0 (* init-y factor))
                 (deltax (floor n gcd-mn))
                 (deltay (floor m gcd-mn))
                 (k-min1 (get-minimum-factor x0 deltax))
                 (k-min2 (get-minimum-factor y0 (- deltay)))
                 (x1 (+ x0 (* k-min1 deltax)))
                 (y1 (- y0 (* k-min1 deltay)))
                 (x2 (+ x0 (* k-min2 deltax)))
                 (y2 (- y0 (* k-min2 deltay))))
            (cond ((and (>= y1 0)
                        (<= (+ x1 y1) sup))
                   (cons x1 y1))
                  ((and (>= x2 0)
                        (<= (+ x2 y2) sup))
                   (cons x2 y2))
                  (t nil)))))))
