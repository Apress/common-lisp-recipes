;;; Copyright (c) 2015, Edmund Weitz.  All rights reserved.

;;; This is example code for the book "Common Lisp Recipes" and meant
;;; to be used with something like (from SLIME) C-M-x or C-c C-c.
;;; See the book for more information.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (error "This code is not meant to be used with LOAD or COMPILE-FILE."))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defparameter *hours* (make-array '(365 24)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setf (aref *hours* 41 2) "foo")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(let ((a (make-array '(4 5 6))))
  (list (array-total-size a)
        (array-rank a)
        (array-dimensions a)
        (array-dimension a 1)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun my-array-dimension (array i)
  (nth i (array-dimensions array)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun my-array-rank (array)
  (length (array-dimensions array)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun my-array-total-size (array)
  (reduce #'* (array-dimensions array) :initial-value 1))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(let ((a (make-array 10 :fill-pointer 3)))
  (list (array-total-size a)
        (length a)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(make-array '(8 8) :initial-element #\x)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(make-array '(2 3) :initial-contents '((2 3 5) (7 11 13)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(make-array '(2 3) :initial-contents '((1 2 3) (4 5 6)))
(make-array '(3 2) :initial-contents '((1 2) (3 4) (5 6)))
(make-array '(2 3 2)
            :initial-contents '(((1 2) (3 4) (5 6))
                                ((7 8) (9 10) (11 12))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(make-array '(2 3) :initial-contents '#(#(1 2 3) #(4 5 6)))
(make-array '(2 3) :initial-contents '#((1 2 3) (4 5 6)))
(make-array '(2 3) :initial-contents '#(#(1 2 3) (4 5 6)))
(make-array '(2 3) :initial-contents '((1 2 3) #(4 5 6)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(let ((a (make-array '(4 4) :initial-element (list 1 2 3))))
  (setf (second (aref a 0 1)) 42)
  (aref a 2 2))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(let ((a (make-array '(4 4))))
  ;; initialize array
  (dotimes (i 16)
    (setf (row-major-aref a i) (list 1 2 3)))
  ;; now the same test as above
  (setf (second (aref a 0 1)) 42)
  (aref a 2 2))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(let ((a (make-array '(20 10))))
  (dotimes (i 20)
    (dotimes (j 10)
      (setf (aref a i j) (* i j))))
  (list (aref a 6 7)
        (row-major-aref a 67)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(make-array '(3 2 4) :initial-contents '((( 2  3  5  7)
                                          (11 13 17 19))
                                         ((23 29 31 37)
	                                  (41 43 47 53))
                                         ((59 61 67 71)
                                          (73 79 83 89))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(make-array '(4 6) :initial-contents '(( 2  3  5  7 11 13)
                                       (17 19 23 29 31 37)
                                       (41 43 47 53 59 61)
                                       (67 71 73 79 83 89)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(let ((a (make-array '(5 10 20))))
  (dotimes (i 5)
    (dotimes (j 10)
      (dotimes (k 20)
        (setf (aref a i j k) (* i j k)))))
  (list (aref a 2 3 7)
        (row-major-aref a (array-row-major-index a 2 3 7))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(let ((a (make-array 10 :fill-pointer 3)))
  (print (length a))
  (vector-push 42 a)
  (print (length a))
  (aref a 3))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(let ((a (make-array 30
                     :initial-contents
                       (loop for i below 30 collect i)
                     :fill-pointer 20)))
  (print (aref a 23))
  (find 23 a))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(let ((a (make-array 10 :fill-pointer 0)))
  (print (length a))
  (dotimes (i 3)
    (vector-push (* i i) a))
  (list (length a)
        (vector-pop a)
        (length a)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun adjust-test (adjustable)
  (let* ((a (make-array '(4 6)
                        :initial-element 42
                        :adjustable adjustable))
         (b (adjust-array a '(5 5) :initial-element 23)))
    (list (array-dimensions b)
          (eq a b)
          (array-dimensions a)
          (aref b 1 1)
          (aref b 4 4))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun my-adjust-array (old-array new-dimensions &key initial-element)
  (let ((new-array (make-array new-dimensions
                               :initial-element initial-element))
        (copy-dimensions (mapcar #'min new-dimensions
                                 (array-dimensions old-array))))
    (labels ((copy-elements (indices dimensions)
               (if dimensions
                 (dotimes (i (first dimensions))
                   (copy-elements (cons i indices) (rest dimensions)))
                 (setf (apply #'aref new-array (reverse indices))
                       (apply #'aref old-array (reverse indices))))))
      (copy-elements nil copy-dimensions)
      new-array)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(let* ((a (make-array '(5 5)))
       (b (make-array '(3 6)
                      :displaced-to a
                      :displaced-index-offset 4)))
  (dotimes (i 5)
    (dotimes (j 5)
      (setf (aref a i j) (+ (* 10 (1+ i)) (1+ j)))))
  (print (list (aref a 3 1) (aref b 2 0)))
  (setf (aref b 2 0) 23)
  (aref a 3 1))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(make-array '(100 100) :element-type 'double-float)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(let ((a (make-array 10 :element-type '(unsigned-byte 3))))
  (array-element-type a))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(let* ((a (make-array 3 :initial-contents '(1 2 3)))
       (b (copy-seq a)))
  (setf (aref b 1) 42)
  (list a b (eq a b)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun copy-array (array)
  (let ((dimensions (array-dimensions array)))
    (adjust-array (make-array dimensions
                              :element-type (array-element-type array)
                              :displaced-to array)
                  dimensions)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(let* ((a (make-array 3 :initial-contents (list (list 1 2) 3 4)))
       (b (copy-seq a)))
  (setf (nth 1 (aref a 0)) 42)
  (list a b))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
