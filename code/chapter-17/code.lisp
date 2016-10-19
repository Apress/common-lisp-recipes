;;; Copyright (c) 2015, Edmund Weitz.  All rights reserved.

;;; This is example code for the book "Common Lisp Recipes" and meant
;;; to be used with something like (from SLIME) C-M-x or C-c C-c.
;;; See the book for more information.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (error "This code is not meant to be used with LOAD or COMPILE-FILE."))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun sum (n)
  (loop for i from 1 to n
        sum i))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun sum (n)
  (* 1/2 n (1+ n)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; see file "foo.lisp" in this directory
(time (foo:main 40))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(profile "FOO")
(profile)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(profile foo::foo-3 foo::bar-2 foo::foo-1 foo::foo-2
         foo::baz-1 foo::baz-2 foo::bar-1 foo:main)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(foo:main 40)
(report)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(unprofile "FOO")
(profile)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require :sb-sprof)
(sb-sprof:with-profiling (:report :flat)
  (foo:main 40))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :cl-user)

(defvar *l*
  (loop repeat 10000000
        collect (random 1d0)))

(defun test-1 ()
  (let ((result 0))
    (map nil (lambda (x)
               (incf result (* 2d0 x))) *l*)
    result))

(defun test-2 ()
  (declare (optimize speed))
  (let ((result 0))
    (map nil (lambda (x)
               (incf result (* 2d0 x))) *l*)
    result))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(time (test-1))
(time (test-2))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(time (test-1))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :cl-user)

(defconstant +max+ 10000000)

(defvar *a*
  (make-array +max+ :initial-contents (loop repeat +max+
                                            collect (random 1d0))))

(defun test-1 (max)
  (loop for i below max
        sum (aref *a* i)))

(defun test-2 (max)
  (declare (optimize (safety 0)))
  (loop for i below max
        sum (aref *a* i)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun array-sum (array)
  (loop for i below (length array)
        sum (aref array i)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun array-sum (array)
  (declare (optimize speed))   ; <-- THIS LINE WAS ADDED
  (loop for i below (length array)
        sum (aref array i)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun array-sum (array)
  (declare (:explain :types))  ;; <- for LispWorks or AllegroCL
  (loop for i below (length array)
        sum (aref array i)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun foo-1 (x)
  (let ((result 0))
    (dotimes (i 100000000)
      (incf result x))
    result))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun foo-2 (x)
  (declare (double-float x))   ;; <-- ADDED
  (let ((result 0d0))          ;; <-- CHANGED
    (dotimes (i 100000000)
      (incf result x))
    result))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun foo-3 (x)
  (declare (optimize speed)    ;; <-- ADDED
           (double-float x))
  (let ((result 0d0))
    (dotimes (i 100000000)
      (incf result x))
    result))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun foo-4 (x)
  (declare (optimize speed)
           (double-float x))
  (let ((result 0d0))
    (declare (double-float result))  ;; <-- ADDED
    (dotimes (i 100000000)
      (incf result x))
    result))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(foo-4 42)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun foo-5 (x)
  (declare (optimize speed (safety 0))  ;; <-- ADDED
           (double-float x))
  (let ((result 0d0))
    (declare (double-float result))
    (dotimes (i 100000000)
      (incf result x))
    result))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(foo-5 42)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun matching-p (edges)
  (let ((hash (make-hash-table)))
    (loop for (vertex-1 vertex-2) in edges
          when (or (gethash vertex-1 hash)
                   (gethash vertex-2 hash))
          do (return-from matching-p nil)
          else do (setf (gethash vertex-1 hash) t
                        (gethash vertex-2 hash) t))
    t))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defparameter *edges*
  (cons (list 99999 100000)
        (loop for i below 100000 by 2
              collect (list i (1+ i)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(let ((hash (make-hash-table)))  ;; <-- now outside of the function
  (defun matching-p (edges)
    ;; the following initialization loop is new
    (loop for vertex being the hash-keys of hash
          do (setf (gethash vertex hash) nil))
    ;; the rest is exactly as above
    (loop for (vertex-1 vertex-2) in edges
          when (or (gethash vertex-1 hash)
                   (gethash vertex-2 hash))
          do (return-from matching-p nil)
          else do (setf (gethash vertex-1 hash) t
                        (gethash vertex-2 hash) t))
    t))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defparameter *l* (loop for i below 20 collect i))

(defun foo-1 (list n)
  (let ((dummy list))
    (dotimes (i n)
      (setf dummy (reverse dummy)))
    dummy))

(defun foo-2 (list n)
  (let ((dummy list))
    (dotimes (i n)
      (setf dummy (nreverse dummy)))  ;; <-- note the "N" here
    dummy))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
*l*
(time (foo-1 *l* 99999999))
*l*
(time (foo-2 *l* 99999999))
*l*
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(nreverse *l*)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setf *l* (nreverse *l*))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(loop for i below 100000 sum (/ 1 (expt 1.0001d0 i)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun do-something (pair)
  (+ (first pair) (* 2 (second pair))))

(defun foo (list-1 list-2)
  (loop for a in list-1
        for b in list-2
        sum (do-something (list a b))))

;; does the same as FOO
(defun bar (list-1 list-2)
  (loop for a in list-1
        for b in list-2
        sum (let ((x (list a b)))
              (declare (dynamic-extent x))
              (do-something x))))

;; random test data
(defvar *l-1* (loop for i below 1000000
                    collect (random 100)))

(defvar *l-2* (loop for i below 1000000
                    collect (random 100)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(time (foo *l-1* *l-2*))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(time (bar *l-1* *l-2*))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun three-1 ()
  (list (random 100)
        (random 100)
        (random 100)))

(defun three-2 ()
  (values (random 100)
          (random 100)
          (random 100)))

;; just to make results comparable
(defvar *r* (make-random-state t))

(defun test-1 (n)
  (setf *random-state* (make-random-state *r*))
  (let ((result 0))
    (dotimes (i n result)
      (destructuring-bind (x y z)
          (three-1)
        (incf result (min x y z))))))

(defun test-2 (n)
  (setf *random-state* (make-random-state *r*))
  (let ((result 0))
    (dotimes (i n result)
      (multiple-value-bind (x y z)
          (three-2)
        (incf result (min x y z))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(time (test-1 1000000))
(time (test-2 1000000))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun member* (elt list)
  (cond ((null list) nil)
        ((eql (first list) elt) list)
        (t (member* elt (rest list)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(member* 42 (make-list 10000 :initial-element 41))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun member* (elt list)
  (loop for rest on list
        when (eql (first rest) elt)
        do (return rest)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod apply-transform ((transform number) vector)
  (* transform vector))

(defmethod apply-transform ((transforms list) vector)
  (apply-transform (compound-transform transforms) vector))

(defun compound-transform (transforms)
  ;; or use REDUCE
  (let ((compound-transform 1))
    (dolist (transform transforms)
      (setf compound-transform (* transform compound-transform)))
    compound-transform))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(apply-transform '(1 2 3 4 5) vector)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(apply-transform 120 vector)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-compiler-macro apply-transform (&whole form &environment env
                                        transform vector)
  (cond ((and (constantp transform env) (listp transform))
         `(apply-transform (load-time-value
                            (compound-transform ,transform))
                           ,vector))
        (t form)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(trace compound-transform)
(defun foo (vector)
   (apply-transform '(1 2 3 4 5) vector))
(foo 10)
(compile 'foo)
(foo 10)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun fib (n)
  (if (<= n 1)
    1
    (+ (fib (- n 2)) (fib (- n 1)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(time (fib 42))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(let ((hash (make-hash-table)))
  (defun fib* (n)
    (or (gethash n hash)
        (setf (gethash n hash)
              ;; below is the original algorithm
              (if (<= n 1)
                1
                (+ (fib* (- n 2)) (fib* (- n 1))))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(time (fib* 42))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(fare-memoization:memoize 'fib)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(declaim (inline bar))         ;; <- see (a) in book
(defun bar (i)
  (declare (optimize speed)
           (double-float i))
  (let ((j (+ i 1d0)))
    (sqrt (+ (* i i) (* j j)))))
(declaim (notinline bar))      ;; <- see (b) in book

(defun foo-1 ()
  (declare (optimize speed))
  (let ((x 0d0)
        (i 1d0))
    (declare (double-float x))
    (loop
       (unless (< i 100000000d0)
         (return x))
       (incf x (the double-float (bar i)))
       (incf i 1d0))))

(defun foo-2 ()
  (declare (optimize speed)
           (inline bar))       ;; <- see (c) in book
  (let ((x 0d0)
        (i 1d0))
    (declare (double-float x))
    (loop
       (unless (< i 100000000d0)
         (return x))
       (incf x (bar i))        ;; <- one declaration less
       (incf i 1d0))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(time (foo-1))
(time (foo-2))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun foo-1 ()
  (flet ((bar (i)
           (sqrt (+ (* i i) (* i i)))))
    (loop for k below 42
          collect (bar k))))                          ;; (A)

(defun foo-2 ()
  (flet ((bar (i)
           (sqrt (+ (* i i) (* i i)))))
    (loop for k below 42
          collect (+ (bar k) (bar (+ k 1))))))        ;; (B)

(defun foo-3 ()
  (flet ((bar (i)
           (sqrt (+ (* i i) (* i i)))))
    (declare (inline bar))
    (loop for k below 42
          collect (+ (bar k) (bar (+ k 1))))))        ;; (C)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun dot-product-1 (x-1 x-2 y-1 y-2)
  (+ (* x-1 y-1) (* x-2 y-2)))

(defun dot-product-2 (x-1 x-2 y-1 y-2)
  (declare (optimize (safety 2)
                     (hcl:fixnum-safety 0)))
  (+ (* x-1 y-1) (* x-2 y-2)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(disassemble 'dot-product-1)
(disassemble 'dot-product-2)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defconstant +max+ 10000000)

(defvar *a*
  (let ((a (make-array +max+ :element-type 'double-float)))
    (dotimes (i +max+)
      (setf (aref a i) (random 1d0)))
    a))

(defun foo-1 (a)
  (let ((result 1d0))
    (declare (double-float result))
    (dotimes (i +max+)
      (incf result (the double-float (aref a i))))
    result))

(defun foo-2 (a)
  (declare (type (simple-array double-float (*)) a))
  (let ((result 1d0))
    (declare (double-float result))
    (dotimes (i +max+)
      (incf result (aref a i)))
    result))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(time (foo-1 *a*))
(time (foo-2 *a*))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
