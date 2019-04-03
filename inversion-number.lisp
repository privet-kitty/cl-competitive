;; -*- coding: utf-8 -*-

;;
;; Fehlstandszahl von einen Vektor berechnen
;;

(deftype non-negative-fixnum nil '(integer 0 #.(min most-positive-fixnum array-total-size-limit)))

(declaim (inline %merge))
(defun %merge (l mid r source-vec dest-vec source-ref dest-ref predicate)
  (loop with i = l
        with j = mid
        for idx from l
        when (= i mid)
          do (loop for j from j below r
                   for idx from idx
                   do (setf (aref dest-vec idx) (aref source-vec j)
                            (aref dest-ref idx) (aref source-ref j))
                   finally (return-from %merge t))
        when (= j r)
          do (loop for i from i below mid
                   for idx from idx
                   do (setf (aref dest-vec idx) (aref source-vec i)
                            (aref dest-ref idx) (aref source-ref i))
                   finally (return-from %merge t))
        do (if (funcall predicate (aref source-vec i) (aref source-vec j))
               (setf (aref dest-vec idx) (aref source-vec i)
                     (aref dest-ref idx) (aref source-ref i)
                     i (1+ i))
               (setf (aref dest-vec idx) (aref source-vec j)
                     (aref dest-ref idx) (aref source-ref j)
                     j (1+ j)))))

(declaim (inline merge-sort!))
(defun merge-sort! (vector predicate)
  (declare (vector vector)
           (function predicate))
  "Mischsortieren. Gibt den sortierten Vektor und den endsprechenden Indexvektor
zurück."
  (let* ((len (length vector))
         (vec1 vector)
         (vec2 (make-array len :element-type (array-element-type vector)))
         (idxvec1 (make-array len :element-type 'non-negative-fixnum)) ; Vektor von Indizes
         (idxvec2 (make-array len :element-type 'non-negative-fixnum))
         ;; Was wir brauchen ist die inverse Abbildung von idxvec1.
         ;; hier idxvec2 als Alias wiederverwenden
         (refvec idxvec2))
    (declare (dynamic-extent vec2 idxvec1))
    (dotimes (i len)
      (setf (aref idxvec1 i) i))
    (labels ((%merge-sort (l r merge-to-vec1-p)
               (declare (optimize (speed 3) (safety 0))
                        (non-negative-fixnum l r))
               (if (<= (- r l) 2)
                   (if (funcall predicate (aref vec1 l) (aref vec1 (- r 1)))
                       (unless merge-to-vec1-p
                         (setf (aref vec2 l) (aref vec1 l)
                               (aref vec2 (- r 1)) (aref vec1 (- r 1))
                               (aref idxvec2 l) (aref idxvec1 l)
                               (aref idxvec2 (- r 1)) (aref idxvec1 (- r 1))))
                       (if merge-to-vec1-p
                           (progn
                             (rotatef (aref vec1 l) (aref vec1 (- r 1)))
                             (rotatef (aref idxvec1 l) (aref idxvec1 (- r 1))))
                           (setf (aref vec2 l) (aref vec1 (- r 1))
                                 (aref idxvec2 l) (aref idxvec1 (- r 1))
                                 (aref vec2 (- r 1)) (aref vec1 l)
                                 (aref idxvec2 (- r 1)) (aref idxvec1 l))))
                   (let ((mid (ceiling (+ l r) 2)))
                     (%merge-sort l mid (not merge-to-vec1-p))
                     (%merge-sort mid r (not merge-to-vec1-p))
                     (if merge-to-vec1-p
                         (%merge l mid r vec2 vec1 idxvec2 idxvec1 predicate)
                         (%merge l mid r vec1 vec2 idxvec1 idxvec2 predicate))))))
      (unless (zerop len)
        (%merge-sort 0 len t)
        (dotimes (i len)
          (setf (aref refvec (aref idxvec1 i)) i)))
      (values vec1 refvec))))

(declaim (inline calc-inversion-number!))
(defun calc-inversion-number! (vector non-strict-order)
  "Nur eine nicht strenge Ordnung (wie #'<=) ist zulässig."
  (let* ((invnum 0)
         (b (make-array (length vector) :element-type 'non-negative-fixnum
                                        :initial-element 0))
         (refvec (nth-value 1 (merge-sort! (copy-seq vector) non-strict-order))))
    (declare (non-negative-fixnum invnum))
    (dotimes (i (length vector) invnum)
      (loop for k from 0 to (aref refvec i)
            sum (aref b k) into i-ti of-type non-negative-fixnum
            finally (incf invnum (the non-negative-fixnum (- i i-ti)))
                    (incf (aref b (aref refvec i)) 1)))))

;; (defun calc-inversion-number-with-bubble-sort! (vec non-strict-order)
;;   "PREDICATE darf nicht strenge Ordnung sein."
;;   (loop for end from (length vec) above 0
;;         sum (loop with inv-count = 0
;;                   for i from 0 below (- end 1)
;;                   do (unless (funcall non-strict-order (aref vec i) (aref vec (+ i 1)))
;;                        (rotatef (aref vec i) (aref vec (+ i 1)))
;;                        (incf inv-count))
;;                   finally (return inv-count))))

;; (defun test-sort (low high size sample)
;;   (let* (; (state (sb-ext:seed-random-state 0))
;;          (vec (make-array size :element-type 'fixnum)))
;;     (labels ((random-fixnum ()
;;                (- (random (- high low)) low)))
;;       (loop repeat sample
;;             do (dotimes (idx size)
;;                  (setf (aref vec idx) (random-fixnum)))
;;                (multiple-value-bind (sorted-vec ref)
;;                    (merge-sort! (copy-seq vec) #'<)
;;                  (unless (and (equalp (sort (copy-seq vec) #'<)
;;                                       sorted-vec)
;;                               (equalp (print vec)
;;                                       (print (apply #'vector
;;                                                     (loop for i below size
;;                                                           collect (aref sorted-vec (aref ref i)))))))
;;                    (return-from test-sort nil)))
;;             finally (return t)))))

;; (defun test ()
;;   (let ((vec (make-array 200 :element-type 'fixnum)))
;;     (declare ((simple-array fixnum (200)) vec))
;;     (dotimes (i (length vec)) (setf (aref vec i) (random 20)))
;;     (print (calc-inversion-number! vec #'<=))
;;     (print (calc-inversion-number-with-bubble-sort! vec #'<=))))
