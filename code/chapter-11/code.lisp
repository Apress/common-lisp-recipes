;;; Copyright (c) 2015, Edmund Weitz.  All rights reserved.

;;; This is example code for the book "Common Lisp Recipes" and meant
;;; to be used with something like (from SLIME) C-M-x or C-c C-c.
;;; See the book for more information.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (error "This code is not meant to be used with LOAD or COMPILE-FILE."))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defparameter *counter* 0)
(defvar *thread*)
(progn
  (setq *thread* (bt:make-thread (lambda ()
                                   (sleep 2)
                                   (incf *counter*))))
  (print *thread*)
  (sleep 1)
  *counter*)
;; wait at least a second
*counter*
*thread*
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defparameter *counter* 0)
(let ((thread (bt:make-thread (lambda ()
                                (sleep 5)
                                (incf *counter*)))))
  (print (bt:thread-alive-p thread))
  (sleep 1)
  (bt:destroy-thread thread)
  (sleep 1)
  (print (bt:thread-alive-p thread))
  (sleep 5)
  *counter*)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defparameter *counter* 0)
(let* (please-stop      ;; the flag
       (thread (bt:make-thread
                (lambda ()
                  (loop repeat 5 do (sleep 1)
                        when please-stop do (return)
                        finally (incf *counter*))))))
  (print (bt:thread-alive-p thread))
  (sleep 1)
  (setf please-stop t)  ;; raise the flag
  (sleep 1)
  (print (bt:thread-alive-p thread))
  (sleep 5)
  *counter*)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(loop repeat 4 do (bt:make-thread (lambda () (loop))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defparameter *counter* 0)

(defun test ()
  (loop repeat 100
        do (bt:make-thread
            (lambda ()
              (loop repeat 100000 do (incf *counter*))
              (loop repeat 100000 do (decf *counter*))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defparameter *counter* 0)

(defparameter *lock* (bt:make-lock))

(defun test ()
  (loop repeat 100
        do (bt:make-thread
            (lambda ()
              (loop repeat 100000 do (bt:with-lock-held (*lock*)
                                       (incf *counter*)))
              (loop repeat 100000 do (bt:with-lock-held (*lock*)
                                       (decf *counter*)))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun test ()
  (loop repeat 100
        do (bt:make-thread
            (lambda ()
              (loop repeat 100000 do (sys:atomic-incf *counter*))
              (loop repeat 100000 do (sys:atomic-decf *counter*))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(make-hash-table :synchronized t)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(make-hash-table :single-thread t)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defparameter *a*
  (make-array 1000 :element-type '(signed-byte 4)
                   :initial-element 0))

(defun writer (i)
  (loop repeat 100000 do
        (loop repeat 4 do (incf (aref *a* i)))
        (loop repeat 4 do (decf (aref *a* i)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(mapc 'bt:make-thread
      (list (lambda () (writer 0))
            (lambda () (writer 1))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defparameter *list* (make-list 10))

(defun swap ()
  (let ((last-2 (last *list* 2))
        (new-tail (make-list 5)))
    (setf (cdr (nthcdr 4 *list*)) new-tail
          (cdr last-2) nil)))

(defun writer ()
  (loop repeat 1000000
        do (swap)))

(defparameter *results* nil)

(defun reader ()
  (loop repeat 1000000
        do (pushnew (length *list*) *results*)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(mapc 'bt:make-thread '(writer reader))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defparameter *foo* 42)
(defparameter *results* nil)
(bt:make-thread (lambda () (push *foo* *results*)))
*results*
(let ((*foo* :yo))
  (bt:make-thread (lambda () (push *foo* *results*))))
*results*
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defparameter *foo* 42)
(defparameter *results* nil)
(map nil 'bt:make-thread
     (list (lambda ()
             (let ((*foo* 1))
               (sleep .1)
               (push (cons 1 *foo*) *results*)))
           (lambda ()
             (let ((*foo* 2))
               (sleep .1)
               (push (cons 2 *foo*) *results*)))))
*results*
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(bt:make-thread (lambda () (print 42)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(bt:make-thread (lambda () (print 42 #.*standard-output*)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defparameter *foo* 42)
(defparameter *results* nil)
(let ((bt:*default-special-bindings* '((*foo* . :yo))))
  (bt:make-thread (lambda () (push *foo* *results*))))
*results*
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(let* ((state :not-yet-started)
       (thread (bt:make-thread
                (lambda ()
                  (sleep 3)
                  (setf state :finished)))))
  (bt:join-thread thread)
  state)                  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defparameter *new-result* nil)

(defun producer ()
  (dotimes (i 5)
    (setf *new-result* (* i i))
    (sleep 1))
  (setf *new-result* :done))

(defun consumer ()
  (setf *new-result* nil)
  (bt:make-thread 'producer)
  (loop
   (case *new-result*
     (:done (return))
     ((nil))
     (otherwise (print *new-result*)
                (setf *new-result* nil)))
   (sleep .001)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defparameter *new-result* nil)

(defun producer (cv lock)
  (flet ((set-value-and-notify (new-value)
           (bt:with-lock-held (lock)
             (setf *new-result* new-value)
             (bt:condition-notify cv))))
    (dotimes (i 5)
      (set-value-and-notify (* i i))
      (sleep 1))
    (set-value-and-notify :done)))

(defun consumer ()
  (let ((cv (bt:make-condition-variable))
        (lock (bt:make-lock)))
    (bt:make-thread (lambda () (producer cv lock)))
    (loop
       (bt:with-lock-held (lock)
         (bt:condition-wait cv lock)
         (when (eql *new-result* :done)
           (return))
         (print *new-result*)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun seed ()
  (random 100000000))

(deftype von-neumann ()
  '(integer 0 99999999))

(defun middle-square (seed n)
  (declare (optimize speed)
           (type von-neumann seed)
           (fixnum n))
  (loop for i below n
        for val of-type von-neumann = seed
          then (mod (floor (* val val) 10000) 100000000)
        finally (return val)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defparameter *seeds*
  (coerce (loop repeat 40000 collect (seed)) 'vector))

(defparameter *repetitions*
  (coerce (loop repeat 40000 collect (random 100000)) 'vector))

(defun test ()
  (map 'vector 'middle-square *seeds* *repetitions*))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setf lparallel:*kernel* (lparallel:make-kernel 4))

(defun ptest ()
  (lparallel:pmap 'vector 'middle-square *seeds* *repetitions*))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mult (a b)
  ;; number of bits the larger factor has
  (let ((length (max (integer-length a) (integer-length b))))
    (when (< length 100000)
      ;; numbers are "small"
      (return-from mult (* a b)))
    (let* ((length/2 (floor length 2))  ;; half of the bits
           (mask (1- (ash 1 length/2))) ;; bitmask for right half
           (a1 (ash a (- length/2)))    ;; left half of A
           (a2 (logand a mask))         ;; right half of A
           (b1 (ash b (- length/2)))    ;; left half of B
           (b2 (logand b mask))         ;; right half of B
           (a1*b1 (mult a1 b1))
           (a2*b2 (mult a2 b2))
           (prod3 (mult (+ a1 a2) (+ b1 b2))))
      (+ (ash a1*b1 (* 2 length/2))
         a2*b2
         (ash (+ prod3
                 (- a1*b1)
                 (- a2*b2))
              length/2)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun pmult% (a b tree)
  (let ((length (max (integer-length a) (integer-length b))))
    (when (< length 100000)
      (let ((result (gensym)))
        ;; add function to ptree using name RESULT
        (lparallel:ptree-fn result ()
                            ;; this function has no dependencies
                            (lambda () (* a b))
                            tree)
        ;; return this name
        (return-from pmult% result)))
    (let* ((length/2 (floor length 2))
           (mask (1- (ash 1 length/2)))
           (a1 (ash a (- length/2)))
           (a2 (logand a mask))
           (b1 (ash b (- length/2)))
           (b2 (logand b mask))
           ;; the following three are now symbols instead of numbers
           (a1*b1 (pmult% a1 b1 tree))
           (a2*b2 (pmult% a2 b2 tree))
           (prod3 (pmult% (+ a1 a2) (+ b1 b2) tree))
           (result (gensym)))
      ;; add function to ptree using name RESULT and
      ;; tell lparallel which results this'll depend on
      (lparallel:ptree-fn result (list a1*b1 a2*b2 prod3)
                          (lambda (a1*b1 a2*b2 prod3)
                            (+ (ash a1*b1 (* 2 length/2))
                               a2*b2
                               (ash (+ prod3
                                       (- a1*b1)
                                       (- a2*b2))
                                    length/2)))
                          tree)
      ;; return the name as above
      result)))

(defun pmult (a b)        
  (let ((tree (lparallel:make-ptree)))
    (lparallel:call-ptree (pmult% a b tree) tree)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setf lparallel:*kernel* (lparallel:make-kernel 4))

(defparameter *a* (random (expt 2 1000000)))
(defparameter *b* (random (expt 2 1000000)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(time (defparameter *p1* (mult *a* *b*)))
(time (defparameter *p2* (pmult *a* *b*)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defconstant +sc-nprocessors-onln+ 84)

(cffi:defcfun "sysconf" :long
  (name :int))

(defun get-number-of-processors ()
  (sysconf +sc-nprocessors-onln+))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(cffi:defctype dword :unsigned-long)
(cffi:defctype word :unsigned-short)

(cffi:defcstruct processor-struct
  (processor-architecture word)
  (reserved word))

(cffi:defcunion oem-union
  (oem-ide dword)
  (processor-struct (:struct processor-struct)))

(cffi:defcstruct system-info
  (oem-info (:union oem-union))
  (page-size dword)
  (minimum-application-address :pointer)
  (maximum-application-address :pointer)
  (active-processor-mask (:pointer dword))
  (number-of-processors dword)
  (processor-type dword)
  (allocation-granularity dword)
  (processor-level word)
  (processor-revision word))

(cffi:defcfun ("GetSystemInfo" get-system-info) :void
  (data (:pointer (:struct system-info))))

(defun get-number-of-processors ()
  (cffi:with-foreign-object (info '(:struct system-info))
    (get-system-info info)
    (cffi:foreign-slot-value info '(:struct system-info)
                             'number-of-processors)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
