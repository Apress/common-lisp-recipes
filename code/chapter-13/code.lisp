;;; Copyright (c) 2015, Edmund Weitz.  All rights reserved.

;;; This is example code for the book "Common Lisp Recipes" and meant
;;; to be used with something like (from SLIME) C-M-x or C-c C-c.
;;; See the book for more information.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (error "This code is not meant to be used with LOAD or COMPILE-FILE."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(deftype zahl (&optional from to)
  `(integer ,from ,to))
(deftype small-prime ()
  '(member 2 3 5 7 11 13 17 19))
(defun test (x)
  (typecase x
    ((not zahl) :not-an-integer)
    ((zahl * 1) :primes-are-greater-than-one)
    (small-prime :definitely-prime)
    (otherwise :could-be-prime)))
(mapcar 'test '(two 23.0 -10 17 15485863))
(defun has-simple-name (symbol)
  (let ((name (symbol-name symbol)))
    (and (< (length name) 5)
         (every (lambda (char)
                  (char-not-greaterp #\a char #\z))
                name))))
(deftype simple-symbol ()
  '(and symbol
        (satisfies has-simple-name)))
(mapcar (lambda (thing)
          (typep thing 'simple-symbol))
        (list "foo" 'foo 'foobar 'x42 '|qUUx|))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(deftype zahl () 'integer)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun zahl () 'integer)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(deftype zahl (from to)
  `(integer ,from ,to))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(deftype zahl (&optional from to)
  `(integer ,from ,to))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(deftype my-number (&key from to (exactp t))
  `(,(if exactp 'rational 'real) ,from ,to))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass foo () ())
(defparameter *f* (make-instance 'foo))
(typep *f* 'foo)
(defun test (x)
  (typecase x
    (number (1+ x))
    (foo :foo)
    (otherwise nil)))
(mapcar 'test (list 42 *f* "foo"))
(deftype bar () '(or foo number))
(subtypep 'foo 'bar)
(typep 23 'bar)
(typep *f* 'bar)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod gamma ((x number))
  ;; Taylor series approximation
  ;; see for example http://rosettacode.org/wiki/Gamma_function
  )

(defmethod gamma ((n integer))
  (if (plusp n)
    ;; compute factorial of (1- N) if N is a positive integer
    (loop for i from 1 to (1- n)
          for result = 1 then (* result i)
          finally (return result))
    ;; otherwise use method above
    (call-next-method)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defgeneric factorial (n)
  (:method ((n (eql 0))) 1)
  (:method ((n integer)) (* n (factorial (1- n)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; will signal an error:
(defmethod gamma ((n (integer 1 *)))
  (loop for i from 1 to (1- n)
        for result = 1 then (* result i)
        finally (return result)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; this is fine:
(deftype positive-integer () '(integer 1 *))

;; but this will signal an error:
(defmethod gamma ((n positive-integer))
  (loop for i from 1 to (1- n)
        for result = 1 then (* result i)
        finally (return result)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass foo ()
  ((key :reader key)))

(defun make-key (secret)
  (format nil "~A-~A" secret (random 100)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod initialize-instance :after ((new-object foo) &key secret)
  (setf (slot-value new-object 'key) (make-key secret)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(make-instance 'foo :secret "confidential")
(key *)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass bar ()
  ((range :initarg :range
          :reader range)))
(defun make-bar (begin end)
  (make-instance 'bar
                 :range (loop for i from begin to end
                              collect i)))
(make-bar 3 10)
(range *)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod make-instance :around ((class (eql 'bar)) &key begin end)
  (cond ((and begin end)
         (call-next-method class
                           :range (loop for i from begin to end
                                        collect i)))
        (t (call-next-method))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(range (make-instance 'bar :range '(a b c)))
(range (make-instance 'bar :begin 40 :end 44))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass foo ()
  ((bar)))

(defmethod (setf closer-mop:slot-value-using-class) :after
    (new-value (class standard-class) (object foo) slot)
  (when (eql (closer-mop:slot-definition-name slot) 'bar)
    (print (list :bar-changed-to new-value))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setf (slot-value (make-instance 'foo) 'bar) 42)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass geometry-object () ())
(defclass scalar (geometry-object) ())
(defclass vec (geometry-object) ())
(defclass matrix (geometry-object) ())
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod mult ((factor1 geometry-object) (factor2 vec))
  :use-fast-simd-routines)
(defmethod mult ((factor1 vec) (factor2 matrix))
  :use-fast-simd-routines)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod mult ((factor1 scalar) (factor2 geometry-object))
  :iterate-through-all-entries)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod mult ((factor1 matrix) (factor2 matrix))
  :double-iteration)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(mult (make-instance 'scalar) (make-instance 'vec))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defgeneric mult (factor-1 factor-2)
  (:argument-precedence-order factor-2 factor-1))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(mult (make-instance 'scalar) (make-instance 'vec))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass foo ()
  ((first-access :reader first-access)))
(defparameter *foo* (make-instance 'foo))
(defmethod slot-unbound (class
                         (object foo)
                         (slot-name (eql 'first-access)))
  (setf (slot-value object 'first-access)
        (get-universal-time)))
(get-universal-time)
(first-access *foo*)
(get-universal-time)
(first-access *foo*)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass foo ()
  ((a :initarg :a)
   (b :initarg :b)
   (c :initarg :c)))
(defparameter *object*
  (make-instance 'foo :a 1 :b 2))
(describe *object*)
(defclass bar ()
  ((b :initarg :b)
   (c :initarg :c)
   (d :initarg :d)))
(change-class *object* 'bar)
(describe *object*)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; continued from above with same class definitions
(defparameter *object*
  (make-instance 'foo :a 1 :b 2))
(change-class *object* 'bar :c 3 :d 4)
(describe *object*)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass person1 ()
  ((fname :initarg :fname)
   (lname :initarg :lname)
   (city :initarg :city)))
(defclass person2 ()
  ((name :initarg :name)
   (city :initarg :city)))
(defparameter *batman*
  (make-instance 'person1
                 :fname "Bruce"
                 :lname "Wayne"
                 :city "Gotham City"))
(describe *batman*)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod update-instance-for-different-class
    ((old person1) (new person2) &key)
  (setf (slot-value new 'name)
        (format nil "~A ~A"
                (slot-value old 'fname)
                (slot-value old 'lname))))
(change-class *batman* 'person2)
(describe *batman*)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass foo ()
  ((a :initarg :a)
   (b :initarg :b)
   (c :initarg :c)))
(defparameter *object*
  (make-instance 'foo :a 1 :b 2))
(describe *object*)
(defclass foo ()           ;; <-- same class
  ((b :initarg :b)
   (c :initarg :c)
   (d :initarg :d)))       ;; <-- new slot
(describe *object*)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass person ()
  ((fname :initarg :fname)
   (lname :initarg :lname)
   (city :initarg :city)))
(defparameter *batman*
  (make-instance 'person
                 :fname "Bruce"
                 :lname "Wayne"
                 :city "Gotham City"))
(describe *batman*)
(defclass person ()          ;; <-- same class
  ((name :initarg :name)
   (city :initarg :city)))
(defmethod update-instance-for-redefined-class
    ((object person) added deleted plist &key)
  (declare (ignore added deleted))
  (setf (slot-value object 'name)
        (format nil "~A ~A"
                (getf plist 'fname)
                (getf plist 'lname))))
(describe *batman*)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass point ()
  ((x :initarg :x
      :reader x)
   (y :initarg :y
      :reader y)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod distance ((p1 point) (p2 point))
  (sqrt (+ (expt (- (x p1) (x p2)) 2)
           (expt (- (y p1) (y p2)) 2))))

(defmethod distance-from-origin ((p point))
  (distance #.(make-instance 'point :x 0 :y 0) p))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod make-load-form ((p point) &optional environment)
  (declare (ignore environment))
  `(make-instance 'point :x ,(x p) :y ,(y p)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod make-load-form ((p point) &optional environment)
  (declare (ignore environment))
  (make-load-form-saving-slots p :slot-names '(x y)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; continued from above
(make-load-form (make-instance 'point :x 0 :y 0))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass quadrilateral () ())
(defclass kite (quadrilateral) ())
(defclass parallelogram (quadrilateral) ())
(defclass trapezoid (quadrilateral) ())
(defclass rhombus (kite parallelogram) ())
(defclass rectangle (parallelogram trapezoid) ())
(defclass square (rectangle rhombus) ())
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defgeneric symmetries (shape)
  (:method-combination append))

(defmethod symmetries append ((shape quadrilateral))
  '(:identity))
(defmethod symmetries append ((shape kite))
  '(:reflection-horizontal))
(defmethod symmetries append ((shape parallelogram))
  '(:rotation-180-degrees))
(defmethod symmetries append ((shape rhombus))
  '(:reflection-vertical))
(defmethod symmetries append ((shape rectangle))
  '(:reflection-vertical :reflection-horizontal))
(defmethod symmetries append ((shape square))
  '(:rotation-90-degrees :rotation-270-degrees
    :reflection-diagonal-1 :reflection-diagonal-2))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(symmetries (make-instance 'rectangle))
(symmetries (make-instance 'rhombus))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(append '(:reflection-vertical)   ;; from RHOMBUS
        '(:reflection-horizontal) ;; from KITE
        '(:rotation-180-degrees)  ;; from PARALLELOGRAM
        '(:identity))             ;; from QUADRILATERAL
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defgeneric symmetries (shape)
  (:method-combination append :most-specific-last))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(symmetries (make-instance 'square))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod symmetries :around ((shape quadrilateral))
  (remove-duplicates (call-next-method)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun set-union (&rest sets)
  (reduce 'union sets))

(define-method-combination set-union)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-method-combination set-union :operation set-union)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defgeneric symmetries (shape)
  (:method-combination set-union))

(defmethod symmetries set-union ((shape quadrilateral))
  '(:identity))
(defmethod symmetries set-union ((shape kite))
  '(:reflection-horizontal))
(defmethod symmetries set-union ((shape parallelogram))
  '(:rotation-180-degrees))
(defmethod symmetries set-union ((shape rhombus))
  '(:reflection-vertical))
(defmethod symmetries set-union ((shape rectangle))
  '(:reflection-vertical :reflection-horizontal))
(defmethod symmetries set-union ((shape square))
  '(:rotation-90-degrees :rotation-270-degrees
    :reflection-diagonal-1 :reflection-diagonal-2))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun number-qualifier-p (method-qualifiers)
  (and (numberp (first method-qualifiers))
       (null (rest method-qualifiers))))

(define-method-combination weighted-sum
    (&optional (initial-value 0))
    ((instead-methods (:instead))
     (sum-methods number-qualifier-p))
  (cond (instead-methods `(call-method ,(first instead-methods)
                                       ,(rest instead-methods)))
        (t `(let ((sum ,initial-value))
              ,@(loop for method in sum-methods
                      collect `(incf sum
                                     (* ,(first
                                          (method-qualifiers method))
                                        (call-method ,method))))
              sum))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defgeneric foo (number)
  (:method-combination weighted-sum 42))

(defmethod foo 2 ((x real)) x)
(defmethod foo 10 ((x rational)) x)
(defmethod foo :instead ((x fixnum)) x)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(foo 10.0)
(foo 1/2)
(foo 23)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass logged-class (standard-class)
  ((creation-log :initform nil
                 :accessor creation-log)))

(defmethod closer-mop:validate-superclass
    ((class logged-class)
     (superclass standard-class))
  t)

(defmethod make-instance :around ((class logged-class) &key)
  (let ((new-object (call-next-method)))
    (push (format nil "~A created at ~A."
                  new-object (get-universal-time))
          (creation-log class))
    new-object))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass foo () () (:metaclass logged-class))
(make-instance 'foo)
(make-instance 'foo)
(creation-log (class-of *))
(defclass bar () () (:metaclass logged-class))
(make-instance 'bar)
(creation-log (class-of *))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass quux () ())
(make-instance 'quux)
(class-of *)
(class-of *)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass foo ()
  ((creation-log :initform nil
                 :accessor creation-log
                 :allocation :class)))  ;; <-- this is new

(defmethod make-instance :around
    ((class (eql 'foo)) &key)           ;; <-- one specific class
  (let ((new-object (call-next-method)))
    (push (format nil "~A created at ~A."
                  new-object (get-universal-time))
          (creation-log new-object))    ;; <-- hangs off class
    new-object))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
