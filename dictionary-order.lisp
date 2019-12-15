;; from UIOP:LEXICOGRAPHIC<
(declaim (inline dict<))
(defun dict< (element< x y)
  "Lexicographically compare two sequences using the function element< to
compare elements. element< is a strict total order; the resulting order on X and
Y will also be strict."
  (etypecase x
    (list
     (check-type y list)
     (labels ((recur (x y)
                (cond ((null y) nil)
                      ((null x) t)
                      ((funcall element< (car x) (car y)) t)
                      ((funcall element< (car y) (car x)) nil)
                      (t (recur (cdr x) (cdr y))))))
       (recur x y)))
    (vector
     (check-type y vector)
     (let ((xlen (length x))
           (ylen (length y)))
       (labels ((recur (i j)
                  (declare ((integer 0 #.most-positive-fixnum) i j))
                  (cond ((= j ylen) nil)
                        ((= i xlen) t)
                        ((funcall element< (aref x i) (aref y i)) t)
                        ((funcall element< (aref y i) (aref x i)) nil)
                        (t (recur (+ i 1) (+ j 1))))))
         (recur 0 0))))))
