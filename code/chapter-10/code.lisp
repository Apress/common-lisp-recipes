;;; Copyright (c) 2015, Edmund Weitz.  All rights reserved.

;;; This is example code for the book "Common Lisp Recipes" and meant
;;; to be used with something like (from SLIME) C-M-x or C-c C-c.
;;; See the book for more information.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (error "This code is not meant to be used with LOAD or COMPILE-FILE."))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(eql '(a (b c) (3/4 (d) e))
     '(a (b c) (3/4 (d) e)))
(tree-equal '(a (b c) (3/4 (d) e))
            '(a (b c) (3/4 (d) e))
            :test 'eq)
(tree-equal '(a (b c) (3/4 (d) e))
            (list 'a '(b c) (list 3/4 '(d) 'e))
            :test 'eql)
(tree-equal '(a (b c) (3/4 (d) e))
            '(a (b c) (3/4 (d)))
            :test 'eql)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(equal '(1 #\a a) (list 1 #\a 'a))
(equal '(1 #\a a) (list 1 #\A 'a))
(eql "abc" "abc")
(equal "abc" "abc")
(equal "abc"
       (make-array 3
                   :element-type 'character
                   :initial-contents (list #\a #\b #\c)))
(equal (make-array 3
                   :element-type 'fixnum
                   :initial-contents (list 1 2 3))
       (make-array 3
                   :element-type 'fixnum
                   :initial-contents (list 1 2 3)))
(equal "abc" "Abc")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(equal #p"Test" #p"test")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(equalp "abc" "Abc")
(equalp (make-array 3
                    :element-type 'fixnum
                    :initial-contents (list 1 2 3))
        (make-array 3
                    :element-type 'fixnum
                    :initial-contents (list 1 2 3)))
(flet ((test-hash ()
         (let ((hash (make-hash-table)))
           (setf (gethash 42 hash) 'foo)
           hash)))
  (list (equal (test-hash) (test-hash))
        (equalp (test-hash) (test-hash))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(case input
  (0 (stop-processing))
  (1 (accelerate))
  (2 (turn-left))
  (3 (turn-right)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defconstant +stop-command+ 0)
(defconstant +speed-up-command+ 1)
(defconstant +left-command+ 2)
(defconstant +right-command+ 3)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(case input
  (+stop-command+ (stop-processing))
  (+speed-up-command+ (accelerate))
  (+left-command+ (turn-left))
  (+right-command+ (turn-right)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(case input
  (#.+stop-command+ (stop-processing))
  (#.+speed-up-command+ (accelerate))
  (#.+left-command+ (turn-left))
  (#.+right-command+ (turn-right)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(case whatever
  (42 (do-something))
  (#\Z (do-something-else))
  (foo (do-something)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun foo (&key (arg 23))
  (list arg))
(foo :arg 42)
(foo)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun bar (&key ((:arg argument) 23))
  (list argument))
(bar :arg 42)
(bar)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun quux (&key ((:stream *standard-output*)
                   *standard-output*))
  (princ 42))
(quux)
(with-output-to-string (out)
  (quux :stream out))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun baz (&key ((foo bar) 42))
  (list bar))
(baz 'foo 23)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun color (&key (red 0.0) (green 0.0) (blue 0.0))
  (list red green blue))
(color :red 0.3 :blue 0.4)
(defun pure-color (which value)
  (color which value))
(pure-color :red 0.7)
(pure-color :green 0.1)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(let ((counter 0))
  (defun my-count ()
    (print (incf counter))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(let ((counter 0))
  (defun reset-counter ()
    (setf counter 0))
  
  (defun my-count ()
    (print (incf counter))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(my-count)
(my-count)
(my-count)
(reset-counter)
(my-count)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(get-internal-real-time)
;; the file "foo.lisp" can be found in this directory
(compile-file "foo.lisp")
(get-internal-real-time)
(load **)
(get-internal-real-time)
(test)
(get-internal-real-time)
(test)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(get-internal-real-time)
(defun test-2 ()
  (print (load-time-value (get-internal-real-time))))
(get-internal-real-time)
(test-2)
(test-2)
(get-internal-real-time)
(compile 'test-2)
(get-internal-real-time)
(test-2)
(test-2)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(let ((counter 0))
  (defun test ()
    (print (load-time-value (format nil "~B" counter)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defadvice (sqrt no-complex-roots :around)
    (real)
  (if (minusp real)
    nil
    (call-next-advice real)))  ;; <- call original SQRT
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(sqrt 3d0)
(sqrt -3d0)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(remove-advice 'sqrt 'no-complex-roots)
(sqrt -3d0)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(time (loop repeat 2000 sum most-positive-fixnum))
(defadvice (time keep-it-short :around)
    (form env)
  `(let* (result
          (output
           (with-output-to-string (*trace-output*)
             (setf result ,(call-next-advice form env)))))
     (format *trace-output* "~A"
             (subseq output (search "User time" output)))
     result))
(time (loop repeat 2000 sum most-positive-fixnum))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro swap (var-1 var-2)
  (let ((temp (gensym)))
    `(let ((,temp ,var-1))
       (setf ,var-1 ,var-2
             ,var-2 ,temp)
       (values))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defparameter *a* 42)
(defparameter *b* 23)
(swap *a* *b*)
(list *a* *b*)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(let ((list (list 23 42)))
  (swap (first list) (second list))
  list)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(let ((list (list 23 42)))
  (swap (nth (print 0) list) (second list))
  list)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(let ((list (list 23 42)))
  (rotatef (nth (print 0) list) (second list))
  list)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(let ((arr (make-array 4 :initial-contents (list 1 2 3 4)))
      (list (list 10 20 30))
      (var 42))
  (print (list arr list var))
  (rotatef (aref arr 0)
           (elt list 2)
           var
           (second list))
  (list arr list var))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(let ((arr (make-array 4 :initial-contents (list 1 2 3 4)))
      (list (list 10 20 30))
      (var 42))
  (print (list arr list var))
  (print (shiftf (aref arr 0)
                 (elt list 2)
                 var
                 (second list)))
  (list arr list var))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defparameter *latin-numbers* (make-hash-table :test 'equal))
(setf (gethash "II" *latin-numbers*) "duo"
      (gethash "III" *latin-numbers*) "tres"
      (gethash "VI" *latin-numbers*) "sex")
(defun number-word (number)
  (gethash (format nil "~@R" number) *latin-numbers*))
(number-word 3)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun (setf number-word) (new-value number)
  (setf (gethash (format nil "~@R" number) *latin-numbers*)
        new-value))
(setf (number-word 9) "novem")
(number-word 9)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun set-number-word (number new-value)
  (setf (gethash (format nil "~@R" number) *latin-numbers*)
        new-value))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defsetf number-word set-number-word)
(setf (number-word 7) "septem")
(number-word 7)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defparameter *english-numbers* (make-hash-table :test 'equal))

(setf (gethash "one" *english-numbers*) 1
      (gethash "two" *english-numbers*) 2
      (gethash "fifteen" *english-numbers*) 15)

(defun word-number (word)
  (values (gethash word *english-numbers*)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-setf-expander word-number (word)
  (let ((new-value-var (gensym)))
    (values nil
            nil
            `(,new-value-var)
            `(setf (gethash ,word *english-numbers*)
                   ,new-value-var)
            `(word-number ,word))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(word-number "one")
(word-number "eight")
(setf (word-number "eight") 8)
(word-number "eight")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(word-number "fifteen")
(setf (ldb (byte 1 0) (word-number "fifteen")) 0)
(word-number "fifteen")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setf (ldb (byte 1 0) (word-number (print "fifteen"))) 0)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-setf-expander word-number (word)
  (let ((word-var (gensym))                               ;; <- new
        (new-value-var (gensym)))
    (values `(,word-var)                                  ;; <- new
            `(,word)                                      ;; <- new
            `(,new-value-var)
            `(setf (gethash ,word-var *english-numbers*)  ;; <- changed
                   ,new-value-var)
            `(word-number ,word-var))))                   ;; <- changed
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(word-number "fifteen")
(setf (ldb (byte 1 0) (word-number (print "fifteen"))) 1)
(word-number "fifteen")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defparameter *alist* (list (cons 'donald-duck 'duckburg)
                            (cons 'superman 'metropolis)
                            (cons 'batman 'gotham-city)))
(defun cdr-assoc (item alist)
  (cdr (assoc item alist)))
(cdr-assoc 'batman *alist*)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-setf-expander cdr-assoc (item alist)
  (let ((item-var (gensym))
        (cons-found (gensym))
        (alist-var (gensym))
        (new-value-var (gensym)))
    (values `(,item-var ,alist-var ,cons-found)
            `(,item ,alist (assoc ,item-var ,alist-var))
            `(,new-value-var)
            `(cond (,cons-found
                    (setf (cdr ,cons-found) ,new-value-var))
                   (t
                    (setf ,alist
                          (acons ,item-var ,new-value-var ,alist-var))
                    ,new-value-var))
            `(cdr ,cons-found))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(cdr-assoc 'donald-duck *alist*)
(setf (cdr-assoc 'donald-duck *alist*) 'entenhausen)
(cdr-assoc 'donald-duck *alist*)
(setf (cdr-assoc 'spider-man *alist*) 'new-york-city)
(cdr-assoc 'spider-man *alist*)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defparameter *container* (list :whatever *alist*))
(cdr-assoc 'daredevil (nth 1 *container*))
(setf (cdr-assoc 'daredevil (nth (print 1) *container*))
      'new-york-city)
(cdr-assoc 'daredevil (nth 1 *container*))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-setf-expander cdr-assoc (item alist &environment env)
  (multiple-value-bind (temp-vars temp-forms
                        store-vars
                        setter-form getter-form)
      (get-setf-expansion alist env)
    (let ((item-var (gensym))
          (cons-found (gensym))
          (new-value-var (gensym)))
      (values `(,@temp-vars ,item-var ,cons-found)
              `(,@temp-forms ,item (assoc ,item-var ,getter-form))
              `(,new-value-var)
              `(cond (,cons-found
                      (setf (cdr ,cons-found) ,new-value-var))
                     (t
                      (let ((,(first store-vars)
                             (acons ,item-var ,new-value-var
                                    ,getter-form)))
                        ,setter-form
                        ,new-value-var)))
              `(cdr ,cons-found)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro ashf (integer-place count)
  `(setf ,integer-place (ash ,integer-place ,count)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defparameter *x* 8)
(ashf *x* -1)
*x*
(defparameter *arr* (make-array 10 :initial-element 16))
(ashf (aref *arr* (print 1)) -1)
(aref *arr* 1)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-modify-macro ashf (count) ash)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defparameter *superheroes*
  (list (list 'superman 'clark-kent 'metropolis)
        (list 'batman 'bruce-wayne 'gotham-city)))

(defun superhero-info (hero database)
  (let ((entry (assoc hero database)))
    (values (second entry)
            (third entry))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(superhero-info 'superman *superheroes*)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-setf-expander superhero-info (hero database &environment env)
  (multiple-value-bind (temp-vars temp-forms
                        store-vars
                        setter-form getter-form)
      (get-setf-expansion database env)
    (let ((hero-var (gensym))
          (entry-found (gensym))
          (new-value-vars (list (gensym) (gensym))))
      (values `(,@temp-vars ,hero-var ,entry-found)
              `(,@temp-forms ,hero (assoc ,hero-var ,getter-form))
              `,new-value-vars
              `(cond (,entry-found
                      (setf (cdr ,entry-found) (list ,@new-value-vars)))
                     (t
                      (let ((,(first store-vars)
                             (cons (list ,hero-var ,@new-value-vars)
                                   ,getter-form)))
                        ,setter-form
                        (values ,@new-value-vars))))
              `(values-list (rest ,entry-found))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(superhero-info 'spider-man *superheroes*)
(setf (superhero-info 'spider-man *superheroes*)
      (values 'peter-parker
              'new-york-city))
(superhero-info 'spider-man *superheroes*)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro do-something (input)
  (if (constantp input)
    `(do-something-at-load-time ,input)
    `(do-something-at-run-time ,input)))

(defun do-something-at-load-time (input)
  (declare (ignore input))
  'did-something-at-load-time)

(defun do-something-at-run-time (input)
  (declare (ignore input))
  'will-do-something-at-run-time)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(do-something 42)
(defconstant +foo+ 42)
(do-something +foo+)
(let ((foo 42))
  (do-something foo))
(symbol-macrolet ((foo +foo+))
  (do-something foo))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro do-something (input
                        &environment env)  ;; <- added env parameter
  (if (constantp input env)                ;; <- used it
    `(do-something-at-load-time ,input)
    `(do-something-at-run-time ,input)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(symbol-macrolet ((foo +foo+))
  (do-something foo))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun quick-approximation (arg)
  (declare (ignore arg))
  0)

(defun slow-and-exact (arg)
  (declare (ignore arg))
  1)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-compiler-macro slow-and-exact (&whole form &environment env
                                       input)
  (if (eql 3 (second
              (assoc 'speed
                     (introspect-environment:declaration-information
                      'optimize env))))
      `(quick-approximation ,input)
      form))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun test-1 (input-list)
  (loop for input in input-list
        sum (slow-and-exact input)))

(defun test-2 (input-list)
  (declare (optimize speed))
  (loop for input in input-list
        sum (slow-and-exact input)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(test-1 (list 1 2 3))
(test-2 (list 1 2 3))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-compiler-macro computation (&whole form &environment env
                                    input)
  (if (eql 'fixnum
           (cdr
            (assoc 'type
                   (nth-value
                    2
                    (introspect-environment:variable-information
                     input env)))))
      `(locally (declare (notinline computation))
         (the fixnum (computation ,input)))
      form))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun test-1 (arg)
  (+ 42 (computation arg)))

(defun test-2 (arg)
  (declare (fixnum arg))
  (+ 42 (computation arg)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (defun foo () 42)

#|
(defun bar () 43)

(defun baz () 44)
|#

#+(or)
(defun quux () 45)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#+(or) #+(or) (foo) (bar)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
