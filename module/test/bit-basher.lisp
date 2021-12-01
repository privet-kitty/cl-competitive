(defpackage :cp/test/bit-basher
  (:use :cl :fiveam :cp/bit-basher)
  (:import-from :cp/test/base #:base-suite))
(in-package :cp/test/bit-basher)
(in-suite base-suite)

(defparameter *seq* #*01010010110011100111111000011111101011101110110101011100001100011111011110011101000010010001101110110101110000100010101011101001111011110111)
(defparameter *seq+1* #*00101001011001110011111100001111110101110111011010101110000110001111101111001110100001001000110111011010111000010001010101110100111101111011)
(defparameter *seq+2* #*00010100101100111001111110000111111010111011101101010111000011000111110111100111010000100100011011101101011100001000101010111010011110111101)
(defparameter *seq+3* #*00001010010110011100111111000011111101011101110110101011100001100011111011110011101000010010001101110110101110000100010101011101001111011110)
(defparameter *seq+4* #*00000101001011001110011111100001111110101110111011010101110000110001111101111001110100001001000110111011010111000010001010101110100111101111)
(defparameter *seq+5* #*00000010100101100111001111110000111111010111011101101010111000011000111110111100111010000100100011011101101011100001000101010111010011110111)
(defparameter *seq+10* #*00000000000101001011001110011111100001111110101110111011010101110000110001111101111001110100001001000110111011010111000010001010101110100111)
(defparameter *seq+15* #*00000000000000001010010110011100111111000011111101011101110110101011100001100011111011110011101000010010001101110110101110000100010101011101)
(defparameter *seq+20* #*00000000000000000000010100101100111001111110000111111010111011101101010111000011000111110111100111010000100100011011101101011100001000101010)
(defparameter *seq+25* #*00000000000000000000000000101001011001110011111100001111110101110111011010101110000110001111101111001110100001001000110111011010111000010001)
(defparameter *seq+30* #*00000000000000000000000000000001010010110011100111111000011111101011101110110101011100001100011111011110011101000010010001101110110101110000)
(defparameter *seq+40* #*00000000000000000000000000000000000000000101001011001110011111100001111110101110111011010101110000110001111101111001110100001001000110111011)
(defparameter *seq+50* #*00000000000000000000000000000000000000000000000000010100101100111001111110000111111010111011101101010111000011000111110111100111010000100100)
(defparameter *seq+60* #*00000000000000000000000000000000000000000000000000000000000001010010110011100111111000011111101011101110110101011100001100011111011110011101)
(defparameter *seq+138* #*00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001)
(defparameter *seq+139* #*00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000)
(defparameter *seq+140* #*00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000)

(defparameter *seq+1end0* #*01010010110011100111111000011111101011101110110101011100001100011111011110011101000010010001101110110101110000100010101011101001111011110111)
(defparameter *seq+1end1* #*00010010110011100111111000011111101011101110110101011100001100011111011110011101000010010001101110110101110000100010101011101001111011110111)
(defparameter *seq+1end2* #*00110010110011100111111000011111101011101110110101011100001100011111011110011101000010010001101110110101110000100010101011101001111011110111)
(defparameter *seq+1end136* #*00101001011001110011111100001111110101110111011010101110000110001111101111001110100001001000110111011010111000010001010101110100111101111111)
(defparameter *seq+1end137* #*00101001011001110011111100001111110101110111011010101110000110001111101111001110100001001000110111011010111000010001010101110100111101111011)

(defun zero-vector (template-vector)
  (make-array (length template-vector) :element-type 'bit :initial-element 0))

(defun make-random-bit-vector (size &optional (state *random-state*))
  (let ((vector (make-array size :element-type 'bit)))
    (dotimes (i size)
      (setf (aref vector i) (random 2 state)))
    vector))

(defun bit-lshift-naive (vector delta &optional result-vector)
  (let ((n (length vector)))
    (setq result-vector
          (etypecase result-vector
            (null (make-array (length vector) :element-type 'bit :initial-element 0))
            ((eql t) vector)
            (simple-bit-vector result-vector)))
    (assert (= n (length result-vector)))
    (loop for i from (- n 1) downto delta
          do (setf (aref result-vector i) (aref vector (- i delta))))
    (dotimes (i (min delta n))
      (setf (aref result-vector i) 0))
    result-vector))

(defun bit-rshift-naive (vector delta &optional result-vector)
  (let ((n (length vector)))
    (setq result-vector
          (etypecase result-vector
            (null (make-array n :element-type 'bit :initial-element 0))
            ((eql t) vector)
            (simple-bit-vector result-vector)))
    (assert (= n (length result-vector)))
    (loop for i from delta below n
          do (setf (aref result-vector (- i delta)) (aref vector i)))
    (loop for i from (max 0 (- n delta)) below n
          do (setf (aref result-vector i) 0))
    result-vector))

(test bit-lshift/hand
  ;; basic case
  (is (equalp *seq+1* (bit-lshift (copy-seq *seq*) 1)))
  (is (equalp *seq+1* (bit-lshift *seq* 1 (zero-vector *seq*))))
  (is (equalp *seq+1* (bit-lshift *seq* 1)))
  (is (equalp *seq+2* (bit-lshift *seq* 2)))
  (is (equalp *seq+3* (bit-lshift *seq* 3)))
  (is (equalp *seq+4* (bit-lshift *seq* 4)))
  (is (equalp *seq+5* (bit-lshift *seq* 5)))
  (is (equalp *seq+10* (bit-lshift *seq* 10)))
  (is (equalp *seq+15* (bit-lshift *seq* 15)))
  (is (equalp *seq+20* (bit-lshift *seq* 20)))
  (is (equalp *seq+25* (bit-lshift *seq* 25)))
  (is (equalp *seq+30* (bit-lshift *seq* 30)))
  (is (equalp *seq+40* (bit-lshift *seq* 40)))
  (is (equalp *seq+50* (bit-lshift *seq* 50 (zero-vector *seq*))))
  (is (equalp *seq+60* (bit-lshift *seq* 60 (zero-vector *seq*))))
  (is (equalp *seq+138* (bit-lshift *seq* 138 (zero-vector *seq*))))
  (is (equalp *seq+140* (bit-lshift *seq* 140 (zero-vector *seq*))))
  (is (equalp *seq+140* (bit-lshift *seq* 141 (zero-vector *seq*))))
  (is (equalp *seq+140* (bit-lshift *seq* 890 (zero-vector *seq*))))
  (is (equalp *seq+140* (bit-lshift *seq* 8900000000000000 (zero-vector *seq*))))

  ;; corner case
  (is (eq *seq* (bit-lshift *seq* 0 t)))
  (is (equalp *seq* (bit-lshift *seq* 0 t)))
  (is (not (eql *seq* (bit-lshift *seq* 0))))
  (is (equalp #* (bit-lshift *seq* 1000000000000000000 #*)))
  (is (equalp #* (bit-lshift #* 1000000000000000000 #*)))
  (is (equalp #*00000 (bit-lshift #* 1000000000000000000 (copy-seq #*00010))))
  (is (equalp #*00010 (bit-lshift #* 3 (copy-seq #*00010))))
  (is (equalp #*00000 (bit-lshift #* 4 (copy-seq #*00010))))
  (is (equalp #*10010 (bit-lshift #*1 0 (copy-seq #*00010))))
  (is (equalp #*00000 (bit-lshift #*1 5 (copy-seq #*00010))))

  ;; END argument
  (is (equalp *seq+1end0* (bit-lshift *seq* 1 (copy-seq *seq*) 0)))
  (is (equalp *seq+1end1* (bit-lshift *seq* 1 (copy-seq *seq*) 1)))
  (is (equalp *seq+1end2* (bit-lshift *seq* 1 (copy-seq *seq*) 2)))
  (is (equalp *seq+1end136* (bit-lshift *seq* 1 (copy-seq *seq*) 136)))
  (is (equalp *seq+1end137* (bit-lshift *seq* 1 (copy-seq *seq*) 137)))

  ;; smaller dest-vector
  (is (equalp #*00101 (bit-lshift *seq* 1 #*00000)))
  (is (equalp #*00001 (bit-lshift *seq* 3 #*00000)))
  (is (equalp #*00000 (bit-lshift *seq* 4 #*00000)))
  (is (equalp #*00000 (bit-lshift *seq* 1000000000000000000 #*00000))))


(test bit-rshift/hand
  ;; corner case
  (is (eq *seq* (bit-rshift *seq* 0 t)))
  (is (equalp *seq* (bit-rshift *seq* 0 t)))
  (is (not (eql *seq* (bit-rshift *seq* 0))))
  (is (equalp #* (bit-rshift *seq* 1000000000000000000 #*)))
  (is (equalp #* (bit-rshift #* 1000000000000000000 #*)))
  (is (equalp #*00000 (bit-rshift #* 1000000000000000000 (copy-seq #*00010))))
  (is (equalp #*00000 (bit-rshift #* 3 (copy-seq #*00010))))
  
  ;; smaller dest-vector
  (is (equalp #*10100 (bit-rshift *seq* 1 #*00000)))
  (is (equalp #*10010 (bit-rshift *seq* 3 #*00000)))
  (is (equalp #*00101 (bit-rshift *seq* 4 #*00000)))
  (is (equalp #*00000 (bit-rshift *seq* 1000000000000000000 #*00000))))

(test bit-lshift/random
  (let ((*test-dribble* nil)
        (state (sb-ext:seed-random-state 0)))
    (dolist (size '(0 1 10 100 1000 64 128))
      (dolist (delta '(0 1 10 100 1000 64 128 3))
        (let* ((vec1 (make-random-bit-vector size state))
               (vec2 (copy-seq vec1)))
          (is (equalp (bit-lshift vec1 delta) (bit-lshift-naive vec1 delta)))
          (bit-lshift vec1 delta t)
          (bit-lshift-naive vec2 delta t)
          (is (equalp vec1 vec2)))))
    (dotimes (_ 1000)
      (let* ((size (random 100 state))
             (delta (random 100 state))
             (vec1 (make-random-bit-vector size state))
             (vec2 (copy-seq vec1)))
        (is (equalp (bit-lshift vec1 delta) (bit-lshift-naive vec1 delta)))
        (bit-lshift vec1 delta t)
        (bit-lshift-naive vec2 delta t)
        (is (equalp vec1 vec2))))))

(test bit-rshift/random
  (let ((*test-dribble* nil)
        (state (sb-ext:seed-random-state 0)))
    (dolist (size '(0 1 10 100 1000 64 128))
      (dolist (delta '(0 1 10 100 1000 64 128 3))
        (let* ((vec1 (make-random-bit-vector size state))
               (vec2 (copy-seq vec1)))
          (is (equalp (bit-rshift vec1 delta) (bit-rshift-naive vec1 delta)))
          (bit-rshift vec1 delta t)
          (bit-rshift-naive vec2 delta t)
          (is (equalp vec1 vec2)))))
    (finishes
      (dotimes (_ 1000)
        (let* ((size (random 100 state))
               (delta (random 100 state))
               (vec1 (make-random-bit-vector size state))
               (vec2 (copy-seq vec1)))
          (is (equalp (bit-rshift vec1 delta) (bit-rshift-naive vec1 delta)))
          (bit-rshift vec1 delta t)
          (bit-rshift-naive vec2 delta t)
          (is (equalp vec1 vec2)))))))

(test bitwise-operations/hand
  (is (equalp #* (bit-not! #*)))
  (is (equalp #* (bit-fill! #* 0)))
  (is (equalp #* (bit-fill! #* 1)))
  (is (equalp #*0 (bit-fill! (copy-seq #*0) 0)))
  (is (equalp #*0 (bit-fill! (copy-seq #*1) 0)))
  (is (equalp #*1 (bit-fill! (copy-seq #*0) 1)))
  (is (equalp #*1 (bit-fill! (copy-seq #*1) 1)))
  (is (equalp #*0 (bit-fill! (copy-seq #*0) 1 1)))
  (is (equalp #*1 (bit-fill! (copy-seq #*1) 1 1)))
  (is (equalp #*0 (bit-fill! (copy-seq #*0) 0 0 0)))
  (is (equalp #*1 (bit-fill! (copy-seq #*1) 0 0 0))))

(test bitwise-operations
  (let ((*test-dribble* nil))
    (dolist (size '(0 1 25 64 128 140))
      (let ((target (make-array size :element-type 'bit :initial-element 0))
            (reference (make-array size :element-type 'bit :initial-element 0))
            (state (sb-ext:seed-random-state 0)))
        (finishes
          (dotimes (i 200)
            ;; bit-not!
            (let ((l (random (+ size 1) state))
                  (r (random (+ size 1) state)))
              (unless (<= l r) (rotatef l r))
              (bit-not! target l r)
              (loop for i from l below r
                    do (setf (sbit reference i) (logxor 1 (sbit reference i))))
              (is (equalp target reference)))
            ;; bit-fill!
            (let ((l (random (+ size 1) state))
                  (r (random (+ size 1) state))
                  (bit (random 2)))
              (unless (<= l r) (rotatef l r))
              (bit-fill! target bit l r)
              (loop for i from l below r
                    do (setf (sbit reference i) bit))
              (is (equalp target reference)))
            ;; bit-count
            (let ((l (random (+ size 1) state))
                  (r (random (+ size 1) state)))
              (unless (<= l r) (rotatef l r))
              (is (= (bit-count target l r)
                     (count 1 target :start l :end r))))))))))

(test bit-next/random
  (let ((*test-dribble* nil)
        (*random-state* (sb-ext:seed-random-state 0)))
    (loop for len from 1 to 200 by 3
          do (dolist (density '(0d0 1d-2 2d-1 4d-1 8d-1))
               (let ((vector (make-array len :element-type 'bit :initial-element 0)))
                 (dotimes (i len)
                   (when (< (random 1d0) density)
                     (setf (aref vector i) 1)))
                 ;; bit-next
                 (dotimes (i len)
                   (let* ((pos (random len))
                          (next (bit-next vector pos)))
                     (incf pos)
                     (loop while (and (< pos len)
                                      (zerop (aref vector pos)))
                           do (incf pos))
                     (when (= pos len)
                       (setq pos nil))
                     (is (eql pos next))))
                 ;; bit-prev
                 (dotimes (i len)
                   (let* ((pos (random len))
                          (prev (bit-prev vector pos)))
                     (decf pos)
                     (loop while (and (>= pos 0)
                                      (zerop (aref vector pos)))
                           do (decf pos))
                     (when (< pos 0)
                       (setq pos nil))
                     (is (eql pos prev))))
                 ;; bit-first
                 (let* ((pos1 (position 1 vector))
                        (pos2 (bit-first vector)))
                   (is (eql pos1 pos2)))
                 ;; bit-last
                 (let* ((pos1 (position 1 vector :from-end t))
                        (pos2 (bit-last vector)))
                   (is (eql pos1 pos2))))))))
