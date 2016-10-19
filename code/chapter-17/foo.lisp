(defpackage :foo
  (:use :cl)
  (:export :main))

(in-package :foo)

(defun foo-1 (n)
  (loop for i below (* 1000 n) maximize i))

(defun foo-2 (n)
  (loop for i below n sum i))

(defun foo-3 (n)
  (loop for i below n sum (foo-2 i)))

(defun bar-1 (n)
  (loop for i below n sum (foo-1 i)))

(defun bar-2 (n)
  (loop for i below n sum (foo-3 i)))

(defun baz-1 (n)
  (bar-2 (* 10 n)))

(defun baz-2 (n)
  (if (zerop n)
      (baz-1 1)
      (+ (bar-1 n) (baz-2 (1- n)))))

(defun main (n)
  (loop for i below n
        sum (+ (baz-1 i) (baz-2 i))))
