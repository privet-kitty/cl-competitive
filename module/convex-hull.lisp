;;;
;;; 2D convex hull of points (Monotone Chain Algorithm)
;;; Complexity: O(nlog(n))
;;;

(declaim (inline make-convex-hull!))
(defun make-convex-hull! (points &optional (eps 0))
  "Returns a vector of the vertices comprising the convex hull, which are sorted
in the anticlockwise direction around the perimeter. This function sorts POINTS
as a side effect.

If EPS is non-negative, three vertices in a straight line are excluded (when the
calculation error is within EPS, of course); they are allowed if EPS is
negative.

POINTS := vector of complex number"
  (declare (inline sort)
           (vector points))
  ;; FIXME: the returned vector may contain duplicate vertices in a degenerative
  ;; case: E.g. (make-convex-hull! #(#c(1 2) #c(1 2) #c(1 2) #c(1 2)) 1d-9) |->
  ;; #(#C(1 2) #C(1 2))
  (macrolet ((outer (p1 p2)
               `(let ((c1 ,p1)
                      (c2 ,p2))
                  (- (* (realpart c1) (imagpart c2))
                     (* (imagpart c1) (realpart c2))))))
    (when (<= (length points) 1)
      (return-from make-convex-hull! (copy-seq points)))
    (let* ((n (length points))
           (pos 0)
           (res (make-array (* n 2) :element-type (array-element-type points)))
           (points (sort points (lambda (p1 p2)
                                  (if (= (realpart p1) (realpart p2))
                                      (< (imagpart p1) (imagpart p2))
                                      (< (realpart p1) (realpart p2)))))))
      (declare (fixnum pos))
      (do ((i 0 (+ i 1)))
          ((= i n))
        (loop (if (and (> pos 1)
                       (<= (outer (- (aref res (- pos 1)) (aref res (- pos 2)))
                                  (- (aref points i) (aref res (- pos 1))))
                           eps))
                  (decf pos)
                  (return)))
        (setf (aref res pos) (aref points i))
        (incf pos))
      ;; REVIEW: solution to the above mentioned problem?
      ;; (when (<= (abs (- (aref res (- pos 1)) (aref res (- pos 2)))) eps)
      ;;   (decf pos))
      (let ((tmp-pos pos))
        (do ((i (- n 2) (- i 1)))
            ((< i 0))
          (loop (if (and (> pos tmp-pos)
                         (<= (outer (- (aref res (- pos 1)) (aref res (- pos 2)))
                                    (- (aref points i) (aref res (- pos 1))))
                             eps))
                    (decf pos)
                    (return)))
          (setf (aref res pos) (aref points i))
          (incf pos)))
      (adjust-array res (- pos 1)))))
