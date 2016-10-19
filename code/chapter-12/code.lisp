;;; Copyright (c) 2015, Edmund Weitz.  All rights reserved.

;;; This is example code for the book "Common Lisp Recipes" and meant
;;; to be used with something like (from SLIME) C-M-x or C-c C-c.
;;; See the book for more information.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (error "This code is not meant to be used with LOAD or COMPILE-FILE."))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun my-sqrt (x)
  (check-type x (real 0))
  (sqrt x))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun my-sqrt (list)
  (check-type (first list) (real 0) "a non-negative real number")
  (sqrt (first list)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defparameter *l* (list -9 :whatever))
(my-sqrt *l*)
*l*
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun my-sqrt (x)
  (declare (type (real 0) x))
  (sqrt x))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun dot-product (x y)
  (assert (and (typep x '(or list vector))
               (typep y '(or list vector))
               (= (length x) (length y)))
          (x y)
          "~S and ~S should have been sequences of the same length."
          x y)
  (reduce '+ (map 'list '* x y)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(dot-product '(2 3 4) '(4))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro assert* (test-form &rest other-args)
  (declare (ignorable test-form other-args))
  #-:release
  `(assert ,test-form ,@other-args))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(pushnew :release *features*)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-condition too-expensive (error)
  ((price :initarg :price
          :reader price))
  (:report (lambda (condition stream)
             (format stream "At ~A Euro~:P that's too expensive."
                     (price condition)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(make-condition 'too-expensive :price 42)
(format nil "~A" *)
(error **)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(signal (make-condition 'error))
(list (signal (make-condition 'error)) 42)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(handler-case
    (list (signal (make-condition 'error)) 42)
  (error ()
    (list :foo :bar)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(list (error (make-condition 'error)) 42)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(handler-case
    (list (error (make-condition 'error)) 42)  ;; changed
  (error ()
    (list :foo :bar)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(list (cerror "Proceed." (make-condition 'error)) 42)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(list (warn (make-condition 'warning)) 42)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(signal 'unbound-variable :name 'foo)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(signal (make-condition 'unbound-variable :name 'foo))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(error "~S and ~S don't match." :foo "FOO")

(error (make-condition 'simple-error             ;; <- default type
                       :format-control "~S and ~S don't match."
                       :format-arguments (list :foo "FOO")))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(warn "~S and ~S don't match." :foo "FOO")

(warn (make-condition 'simple-warning            ;; <- default type
                      :format-control "~S and ~S don't match."
                      :format-arguments (list :foo "FOO")))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun test (a b)
  (handler-case
      (/ a b)
    (type-error (condition)
      (format *error-output*
              "Oops, ~S should have been of type ~A."
              (type-error-datum condition)
              (type-error-expected-type condition))
      :no-meaningful-result)
    (division-by-zero ()
      (format *error-output* "This might create black holes!")
      (values))))
(test 42 7)
(test 42 "23")
(test 42 0)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(test most-positive-double-float least-positive-double-float)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defparameter *special* :old)
(defun div (x y)
  (let ((*special* :new))
    (catch 'catch-tag
      (/ x y))))
(defun test (a b)
  (handler-case
      (div a b)
    (type-error (condition)
      (format *error-output*
              "Oops, ~S should have been of type ~A."
              (type-error-datum condition)
              (type-error-expected-type condition))
      *special*)
    (division-by-zero ()
      (format *error-output* "This might create black holes!")
      (throw 'catch-tag -1))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(test 100 "NaN")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(test 42 0)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defparameter *special* :old)

(defun div (x y)
  (let ((*special* :new))
    (catch 'catch-tag
      (/ x y))))

(defun handler-2 (condition)
  (declare (ignore condition))
  (format *error-output* "This might create black holes!")
  (throw 'catch-tag -1))

(defun test (a b)
  (flet ((handler-1 (condition)
           (format *error-output*
                   "Oops, ~S should have been of type ~A."
                   (type-error-datum condition)
                   (type-error-expected-type condition))
           (return-from test *special*)))
    (handler-bind ((type-error #'handler-1)
                   (division-by-zero #'handler-2))
      (div a b))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(test 100 "NaN")
(test 42 0)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(ignore-errors (parse-integer "42"))
(ignore-errors (parse-integer "fourty-two"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-condition not-a-percentage (error)
    ((dividend :initarg :dividend
               :reader dividend)
     (divisor :initarg :divisor
              :reader divisor))
  (:report (lambda (condition stream)
             (format stream "The quotient ~A/~A is not between 0 and 1."
                     (dividend condition) (divisor condition)))))
            
(defun percentage (a b)
  (restart-case
      (let ((ratio (/ a b)))
        (unless (typep ratio '(real 0 1))
          (error 'not-a-percentage :dividend a :divisor b))
        (format nil "~,2F%" (* 100 ratio)))
    (use-other-values (new-a new-b)
      :report "Use two other values instead."
      :interactive (lambda ()
                     (flet ((get-value (name)
                              (format t "~&Enter new value for ~A: "
                                      name)
                              (read)))
                       (list (get-value 'a) (get-value 'b))))
      (format nil "~,2F%" (* 100 (/ new-a new-b))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(percentage 3 7)
(percentage 4 2)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun percentage (a b)
  (restart-case
      (let ((ratio (/ a b)))
        (unless (typep ratio '(real 0 1))
          (error 'not-a-percentage :dividend a :divisor b))
        (format nil "~,2F%" (* 100 ratio)))
    (use-other-values (new-a new-b)
      :report "Use two other values instead."
      :interactive (lambda ()
                     (flet ((get-value (name)
                              (format t "~&Enter new value for ~A: "
                                      name)
                              (read)))
                       (list (get-value 'a) (get-value 'b))))
      (percentage new-a new-b))))                   ;; <-- changed
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(handler-bind ((not-a-percentage (lambda (condition)
                                   (declare (ignore condition))
                                   (invoke-restart 'use-other-values
                                                   1 10))))
  (percentage 4 2))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun divide-by-three (arg)
  (loop (restart-case
            (let ((type-error (make-condition 'type-error
                                              :expected-type 'integer
                                              :datum arg)))
              (with-condition-restarts
                  type-error
                  (list (find-restart 'parse-string))
                (cond ((stringp arg) (error type-error))
                      ((zerop (mod arg 3)) (return (/ arg 3)))
                      (t (error "Not divisible by three.")))))
          (parse-string ()
            (setf arg (parse-integer arg)))
          (increase-value ()
            :test (lambda (condition)
                    (declare (ignore condition))
                    (typep arg 'integer))
            (incf arg))
          (decrease-value ()
            :test (lambda (condition)
                    (declare (ignore condition))
                    (typep arg '(integer 2)))
            (decf arg)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(divide-by-three 2)
(divide-by-three 1)
(divide-by-three "3")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-condition too-many-iterations (error)
  ())

(defun collatz (start &optional (max 10))
  (let ((count 0)
        (value start))
    (loop (incf count)
          (setf value (if (evenp value)
                        (/ value 2)
                        (1+ (* 3 value))))
          (when (= value 1)
            (return))
          (when (>= count max)
            (cerror "Continue trying?" 'too-many-iterations)
            (setf max (* 2 max))))
    (format t "Reached end after ~A iterations." count)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(handler-bind ((too-many-iterations #'continue))
  (collatz 6171))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun my-expt (base power)
  (unless (and (typep power 'integer)
               (typep base '(or rational (complex rational))))
    (warn "Result may have round-off errors."))
  (expt base power))
(my-expt 10 (log 1/4 10))
(handler-bind ((warning #'muffle-warning))
  (my-expt 10 (log 1/4 10)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass fridge ()
  ((door-open-p :initform nil
                :accessor door-open-p)
   (eggs :initform 10
         :accessor eggs)))

(define-condition fridge-error (error)
  ((fridge :initarg :fridge
           :reader fridge)))

(define-condition no-eggs (fridge-error) ())

(defmethod open-door ((fridge fridge))
  (setf (door-open-p fridge) t))

(defmethod close-door ((fridge fridge))
  (setf (door-open-p fridge) nil))

(defmethod remove-egg ((fridge fridge))
  (unless (plusp (eggs fridge))
    (error 'no-eggs :fridge fridge))
  (decf (eggs fridge)))

(defmethod get-some-eggs ((fridge fridge) n)
  (open-door fridge)
  (loop repeat n do (remove-egg fridge))
  (close-door fridge)
  ;; return number of eggs left
  (eggs fridge))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defparameter *fridge* (make-instance 'fridge))
(door-open-p *fridge*)
(get-some-eggs *fridge* 7)
(door-open-p *fridge*)
(handler-bind ((no-eggs #'abort))
  (get-some-eggs *fridge* 4))
(door-open-p *fridge*)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod get-some-eggs ((fridge fridge) n)
  (open-door fridge)
  (unwind-protect
       (loop repeat n do (remove-egg fridge))
    (close-door fridge))
  (eggs fridge))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; continued from above
(setf (eggs *fridge*) 4)
(close-door *fridge*)
(handler-bind ((no-eggs #'abort))
  (get-some-eggs *fridge* 7))
(door-open-p *fridge*)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
