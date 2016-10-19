;;; Copyright (c) 2015, Edmund Weitz.  All rights reserved.

;;; This is example code for the book "Common Lisp Recipes" and meant
;;; to be used with something like (from SLIME) C-M-x or C-c C-c.
;;; See the book for more information.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (error "This code is not meant to be used with LOAD or COMPILE-FILE."))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; see file "test.c" in this directory
(cffi:load-foreign-library "/tmp/test.so")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(cffi:foreign-funcall "power"
                      :double 1.4142135623730951d0
                      :int 2
                      :double)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(cffi:defcfun "power" :double
  (base :double)
  (exponent :int))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(power 2.5457298950218306d0 4)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(cffi:foreign-funcall "hypot"
                      :double 3d0
                      :double 4d0
                      :double)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; see files "test.c" and "test2.c" in this directory
(cffi:load-foreign-library "/tmp/test.so")
(cffi:load-foreign-library "/tmp/test2.so")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(cffi:define-foreign-library test-lib
  (t "/tmp/test.so"))
(cffi:load-foreign-library 'test-lib)
(cffi:defcfun ("power" :library test-lib) :double
  (base :double)
  (exponent :int))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(cffi:define-foreign-library other-test-lib
  (t "/tmp/test2.so"))
(cffi:load-foreign-library 'other-test-lib)
(cffi:defcfun ("power" one-arg-power :library other-test-lib) :double
  (exponent :int))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; see file "swap.c" in this directory
(cffi:defcfun swap :void
  (a :pointer)
  (b :pointer))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defparameter *a* (cffi:foreign-alloc :int))
*a*
(cffi:mem-ref *a* :int)
(setf (cffi:mem-ref *a* :int) 42)
(cffi:mem-ref *a* :int)
(defparameter *b*
  (cffi:foreign-alloc :int :initial-element 23))
(cffi:mem-ref *b* :int)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(swap *a* *b*)
(list (cffi:mem-ref *a* :int)
      (cffi:mem-ref *b* :int))
(cffi:foreign-free *a*)
(cffi:foreign-free *b*)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(cffi:with-foreign-objects ((a :int) (b :int))
  (setf (cffi:mem-ref a :int) 42
        (cffi:mem-ref b :int) 23)
  (print (list (cffi:mem-ref a :int) (cffi:mem-ref b :int)))
  (swap a b)
  (list (cffi:mem-ref a :int) (cffi:mem-ref b :int)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(cffi:defcfun swap :void
  (a (:pointer :int))
  (b (:pointer :int)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(cffi:with-foreign-objects ((a :int) (b :double))
  (setf (cffi:mem-ref a :int) 42
        (cffi:mem-ref b :double) 23d0)
  (print (list (cffi:mem-ref a :int)
               (cffi:mem-ref b :double)))
  (swap a b)
  (list (cffi:mem-ref a :double) (cffi:mem-ref b :int)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defparameter *test*
  (cffi:foreign-alloc :short :initial-contents (list 42 23)))
(cffi:foreign-type-size :short)
(cffi:foreign-type-size :float)
(list (cffi:mem-ref *test* :short)
      (cffi:mem-ref *test* :short 2)  ;; *TEST* + 2 (octets)
      (cffi:mem-ref *test* :float))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; see file "swap.c" in this directory
(fli:register-module 'foo :real-name "/tmp/foo.so")

(fli:define-foreign-function swap
    ((a (:pointer :int))
     (b (:pointer :int)))
  :module 'foo
  :result-type :void)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(fli:with-dynamic-foreign-objects ((a :int :initial-element 42)
                                   (b :int :initial-element 23))
  (swap a b)
  (list (fli:dereference a) (fli:dereference b)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(fli:with-dynamic-foreign-objects ((a :int :initial-element 42)
                                   (b :double :initial-element 23d0))
  (swap a b)
  (list (fli:dereference a) (fli:dereference b)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; see file "sum.c" in this directory
(cffi:defcfun sum :double
  (arr :pointer)
  (size :int))
(defparameter *arr*
  (cffi:foreign-alloc :double
                      :initial-contents
                      (loop for x from 1 to 10
                            collect (float x 1d0))))
(cffi:mem-aref *arr* :double 3)
(sum *arr* 10)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; see file "sum.c" in this directory
(cffi:defcfun "set_arr" :double
  (arr :pointer)
  (index :int)
  (new-value :double))
(cffi:mem-aref *arr* :double 7)
(set-arr *arr* 7 42d0)
(cffi:mem-aref *arr* :double 7)
;; don't forget this!
(cffi:foreign-free *arr*)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defparameter *arr*
  (make-array 10 :element-type 'double-float
              :initial-contents (loop for x from 1d0 to 10d0 by 1d0
                                      collect x)
              :allocation :static))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(fli:define-foreign-function sum
    ((arr :lisp-array)
     (size :int))
  :result-type :double
  :module 'foo)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
*arr*
(aref *arr* 5)
(sum *arr* (length *arr*))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; see file "complex.c" in this directory
(cffi:defcstruct c-complex
  (real :double)
  (imag :double))
(cffi:defcfun "magnitude_squared" :double
  (c :pointer))
(cffi:with-foreign-object (c '(:struct c-complex))
  (setf (cffi:foreign-slot-value c '(:struct c-complex) 'real)
        3d0
        (cffi:foreign-slot-value c '(:struct c-complex) 'imag)
        4d0)
  (sqrt (magnitude-squared c)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; see file "factorial.c" in this directory
(cffi:defcunion result-union
  (rval :double)
  (ival :unsigned-long))

(cffi:defcstruct result-struct
  (exact (:boolean :char))
  (val (:union result-union)))

(cffi:defcfun factorial :void
  (n :int)
  (r :pointer))

(defun fact (n)
  (cffi:with-foreign-object (r '(:struct result-struct))
    (factorial n r)
    (let ((result-union
            (cffi:foreign-slot-value r '(:struct result-struct) 'val)))
      (if (cffi:foreign-slot-value r '(:struct result-struct) 'exact)
        (cffi:foreign-slot-value result-union
                                 '(:union result-union) 'ival)
        (cffi:foreign-slot-value result-union
                                 '(:union result-union) 'rval)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(fact 20)
(fact 23)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; using RESULT-UNION from example above
(defctype r-union (:union result-union))

(defcstruct result-struct
  (exact (:boolean :char))
  (val r-union))

(defctype r-struct (:struct result-struct))

(defun fact (n)
  (with-foreign-object (r 'r-struct)
    (factorial n r)
    (let ((result-union (foreign-slot-value r 'r-struct 'val)))
      (if (foreign-slot-value r 'r-struct 'exact)
          (foreign-slot-value result-union 'r-union 'ival)
          (foreign-slot-value result-union 'r-union 'rval)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(cffi:foreign-alloc '(:struct c-complex))
(cffi:mem-ref * '(:struct c-complex))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; see file "convert.c" in this directory
(cffi:defcfun convert :void
  (in :string)
  (out :pointer))

(defun show (str)
  (cffi:with-foreign-object (arr :uint (* (length str) 2))
    (convert str arr)
    (loop for i from 0
          for c = (cffi:mem-aref arr :uint i)
          until (zerop c)
          collect c)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(show "Läther")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(cffi:defcfun convert :void
  (in (:string :encoding :latin-1))
  (out :pointer))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(show "Läther")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(babel-encodings:list-character-encodings)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; see file "callback.c" in this directory
(cffi:defcallback print-hex :void
    ((n :int))
  (format t "--> ~X" n))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(cffi:foreign-funcall test
                      :int 32
                      :pointer (cffi:callback print-hex)
                      :void)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun power (base exponent)
  (ffi:c-inline (base exponent) (:double :int) :double "{
    int i;
    double result = 1.0;

    for (i = 1; i <= #1; i++)
      result *= #0;
    @(return) = result;
}"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(power 1.4142135623730951d0 2)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(power 2 10)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(cffi:load-foreign-library "Compl.so")
(cffi:load-foreign-library "Compl_wrap.so")
(load (compile-file "compl.lisp"))
(load (compile-file "compl-clos.lisp"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defparameter *c* (make-instance 'compl :r 3d0 :i 4d0))
(magnitude-squared *c*)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(ffi:load-foreign-library "/tmp/Compl.so")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(ffi:clines "#include \"/tmp/Compl.h\"")

(defun mag (real imag)
  (ffi:c-inline (real imag) (:double :double) :double "{
    Compl c = Compl(#0, #1);
    @(return) = c.magnitude_squared();
}"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(sqrt (mag 3 4))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(jclass "java.util.TimeZone")
(jmethod * "getTimeZone" "java.lang.String")
(jstatic * ** "Europe/Berlin")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(jstatic "getTimeZone" "java.util.TimeZone" "Europe/Berlin")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; continued from above
(jnew "java.text.SimpleDateFormat" "yyyy-MM-dd'T'HH:mm'Z'")
(jcall "setTimeZone" * **)
(jcall "format" ** (jnew "java.util.Date"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'jlinker)
(use-package :net.jlinker)
(jlinker-init :jni)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(let* ((time-zone-class (jclass "java.util.TimeZone"))
       (time-zone
        (jstatic (jmethod time-zone-class "getTimeZone"
                                          "java.lang.String")
                 time-zone-class "Europe/Berlin"))
       (date-format-class (jclass "java.text.SimpleDateFormat"))
       (date-format (jnew date-format-class "yyyy-MM-dd'T'HH:mm'Z'")))
  (jcall "setTimeZone" date-format time-zone)
  (jcall (jmethod date-format-class "format" "java.util.Date")
         date-format (jnew "java.util.Date")))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(jstatic "getTimeZone" "java.util.TimeZone" "Europe/Berlin")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require "java-interface")
(use-package :lw-ji)
(init-java-interface
 :jvm-library-path
   ;; where jvm.dll is (if not on PATH)
   "C:/Program Files/Java/jre1.8.0_31/bin/server/jvm.dll"
 ;; optional (for calling Lisp from Java)
 :java-class-path
   (namestring (lispworks-file "etc/lispcalls.jar")))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(call-java-method "java.util.TimeZone.getTimeZone"
                  "Europe/Berlin")
(create-java-object "java.text.SimpleDateFormat"
                    "yyyy-MM-dd'T'HH:mm'Z'")
(call-java-method "java.text.SimpleDateFormat.setTimeZone"
                  * **)
(call-java-method "java.text.SimpleDateFormat.format"
                  ** (create-java-object "java.util.Date"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(call-java-method "getTimeZone" "Europe/Amsterdam")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(import-java-class-definitions "java.util.TimeZone")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(with-output-to-string (out)
  (write-java-class-definitions-to-stream "java.util.TimeZone" out))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; note that we use a string because it's lowercase
(use-package "java.util")
(timezone.gettimezone "Europe/Paris")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defparameter *counter* 0)
 become the "run" method below
(defun add-something ()
  (incf *counter* 42))
(compile *)
(define-lisp-proxy proxy-example
  ;; the interface to implement
  ("java.lang.Runnable"
   ;; we could have more than one method here
   ("run" add-something)))
(create-java-object "java.lang.Thread"
                    ;; "instantiate" the proxy
                    (make-lisp-proxy 'proxy-example))
*counter*
(call-java-method "java.lang.Thread.start" **)
*counter*
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(yason:parse "1")
(yason:parse "2.0")
(yason:parse "\"I am a string\"")
(yason:parse "null")
(yason:parse "true")
(yason:parse "false")
(yason:parse "[1, 2.0, \"foo\", null, false]")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(yason:parse "{\"one\":1, \"two\": 2.0, \"three\":true}")
(loop for key being the hash-keys of *
      using (hash-value value)
      collect (list key value))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(let ((yason:*parse-json-booleans-as-symbols* t))
  (yason:parse "true"))
(let ((yason:*parse-json-arrays-as-vectors* t))
  (yason:parse "[1, 2.0, \"foo\", null, false]"))
(let ((yason:*parse-object-as* :alist))
  (yason:parse "{one:1, two:2.0, \"three\":true}"))
(let ((yason:*parse-object-as* :plist))
  (yason:parse "{one:1, two:2.0, \"three\":true}"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(yason:encode 3)
(yason:encode 3.141)
(yason:encode t)
(yason:encode nil)
(yason:encode '(1 2 #(3 4)))
(let ((hash (make-hash-table)))
  (setf (gethash "42" hash) "forty-two"
        (gethash "one" hash) '(42))
  (yason:encode hash))
(yason:encode-alist '((:42 . 42) (:foo "foo")))
(yason:encode-plist '(:42 42 :foo "foo"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; see file "note.xml" in this directory
(cxml:parse #p"note.xml" (cxml-xmls:make-xmls-builder))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass my-handler (sax:default-handler)
  ((indentation :initform 0
                :accessor indentation)))

(defmethod sax:start-element ((handler my-handler)
                              namespace-uri local-name qname attributes)
  (declare (ignore namespace-uri qname attributes))
  (incf (indentation handler) 2)
  (format t "~VT~A~%" (indentation handler) local-name))

(defmethod sax:end-element ((handler my-handler)
                            namespace-uri local-name qname)
  (declare (ignore namespace-uri qname))
  (decf (indentation handler) 2))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(cxml:parse #p"note.xml" (make-instance 'my-handler))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package :paiprolog)
(<- (father anakin luke))
(<- (father anakin leia))
(<- (father luke ben))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(?- (father ?x leia))
(?- (father anakin ?x))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(<- (child ?x ?y) (father ?y ?x))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(?- (child ?z anakin))
(?- (child ?a ?b))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(<- (grand-child ?x ?z) (child ?x ?y) (child ?y ?z))
(?- (grand-child ?a anakin))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
