(declaim (inline get-2dcumul))
(defun get-2dcumul (cumul-table i0 j0 i1 j1)
  "Returns the cumulative sum of the region given by the rectangle [i0, i1)*[j0,
j1). CUMUL-TABLE must be appropriately initialized beforehand:
i.e. CUMUL-TABLE[i][j] = sum of the region given by the regtangle [0, i)*[0,
j)."
  (+ (- (aref cumul-table i1 j1)
        (aref cumul-table i0 j1)
        (aref cumul-table i1 j0))
     (aref cumul-table i0 j0)))
