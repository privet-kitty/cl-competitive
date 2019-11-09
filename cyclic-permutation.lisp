(declaim (inline decompose-to-cycles))
(defun decompose-to-cycles (permutation)
  "Returns the list of all the cyclic permutations in a given permutation of {0,
1, ..., N-1}"
  (declare (vector permutation))
  (let* ((n (length permutation))
         result
         (visited (make-array n :element-type 'bit :initial-element 0)))
    (dotimes (init n)
      (when (zerop (sbit visited init))
        (push (loop for x = init then (aref permutation x)
                    until (= (sbit visited x) 1)
                    collect x
                    do (setf (sbit visited x) 1))
              result)))
    result))
