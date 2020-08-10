(declaim (inline 2dcumul-get))
(defun 2dcumul-get (cumul-table i0 j0 i1 j1)
  "Returns the cumulative sum of the region given by the rectangle [i0, i1)*[j0,
j1). CUMUL-TABLE must be appropriately initialized beforehand:
i.e. CUMUL-TABLE[i][j] = sum of the region given by the regtangle [0, i)*[0,
j)."
  (+ (- (aref cumul-table i1 j1)
        (aref cumul-table i0 j1)
        (aref cumul-table i1 j0))
     (aref cumul-table i0 j0)))

(declaim (inline 2dcumul-initialize))
(defun 2dcumul-initialize (cumul-table)
  (destructuring-bind (h+1 w+1) (array-dimensions cumul-table)
    (declare ((integer 0 #.most-positive-fixnum) h+1 w+1))
    (let ((h (- h+1 1))
          (w (- w+1 1)))
      (declare ((integer 0 #.most-positive-fixnum) h w))
      (dotimes (i h+1)
        (dotimes (j w)
          (incf (aref cumul-table i (+ j 1))
                (aref cumul-table i j))))
      (dotimes (j w+1)
        (dotimes (i h)
          (incf (aref cumul-table (+ i 1) j)
                (aref cumul-table i j)))))))
