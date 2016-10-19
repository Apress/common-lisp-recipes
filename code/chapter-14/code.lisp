;;; Copyright (c) 2015, Edmund Weitz.  All rights reserved.

;;; This is example code for the book "Common Lisp Recipes" and meant
;;; to be used with something like (from SLIME) C-M-x or C-c C-c.
;;; See the book for more information.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (error "This code is not meant to be used with LOAD or COMPILE-FILE."))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun foo (n)
  (make-list n :initial-element (read)))
(with-input-from-string (stream "42")
  (let ((*standard-input* stream))
    (foo 3)))
(let ((apropos-result
       (with-output-to-string (stream)
         (let ((*standard-output* stream))
           (apropos 'foo)))))
         apropos-result)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(with-input-from-string (*standard-input* "42")
  (foo 3))
(with-output-to-string (*standard-output*)
  (apropos 'foo))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(let ((*print-base* 2))
  (print 10))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(let ((result
       (with-output-to-string (stream)
         (let ((my-standard-output
                 (make-synonym-stream
                  '*standard-output*)))
           (print 42 my-standard-output)
           (let ((*standard-output* stream))
             (print 43 my-standard-output))
           (print 44 my-standard-output)))))
  result)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(progn
 (write-char #\.)
 (sleep 2)
 (write-char #\.)
 (values))
(progn
 (write-char #\.)
 (force-output)
 (sleep 2)
 (write-char #\.)
 (values))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(with-open-file (in "C:/Windows/winhlp32.exe"
                    :element-type '(unsigned-byte 8))
  (file-length in))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun file-at-once (filespec &rest open-args)
  (with-open-stream (stream (apply #'open
                                   filespec
                                   open-args))
    (let* ((buffer
            (make-array (file-length stream)
                        :element-type
                        (stream-element-type stream)
                        :fill-pointer t))
           (position (read-sequence buffer stream)))
      (setf (fill-pointer buffer) position)
      buffer)))
(defun number-of-users ()
  (count #\Newline
         (file-at-once "/etc/passwd"
                       :element-type 'character)))
(number-of-users)
(subseq (hcl:file-string "/etc/passwd") 0 4)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(with-open-file (stream-1 "/tmp/foo1"
                          :direction :output)
  (with-open-file (stream-2 "/tmp/foo2"
                            :direction :output)
    (let ((out (make-broadcast-stream stream-1
                                      stream-2)))
      (format out
              "This line goes to foo1 and foo2~%")
      (format stream-1 "Only foo1~%")
      (format stream-2 "Only foo2~%")
      (format out "Again both files~%"))))
(file-at-once "/tmp/foo1")
(file-at-once "/tmp/foo2")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(with-open-file (log-file-stream "/tmp/log"
                                 :direction
                                 :output)
  (let ((log-stream
         (make-broadcast-stream log-file-stream
                                *error-output*)))
    (format log-stream "An error occurred~%")
    (with-open-file (*error-output* "/tmp/log2"
                                    :direction
                                    :output)
      (format log-stream "Another error occurred~%"))
    (format log-stream "Zounds! Again an error!~%")))
(file-at-once "/tmp/log2")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(with-open-file (log-file-stream "/tmp/log"
                                 :direction
                                 :output
                                 :if-exists
                                 :supersede)
  (let ((log-stream
         (make-broadcast-stream log-file-stream
                                (make-synonym-stream
                                 '*error-output*))))
    (format log-stream "An error occurred~%")
    (with-open-file
        (*error-output* "/tmp/log2"
                        :direction :output
                        :if-exists :supersede)
      (format log-stream "Another error occurred~%"))
    (format log-stream "Zounds! Again an error!~%")))
(file-at-once "/tmp/log2")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun print-some-stuff ()
  (dotimes (i 10) (format t "~R~%" i)))
(print-some-stuff)
(let ((*standard-output* (make-broadcast-stream)))
  (print-some-stuff))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(with-input-from-string (s "(41 42 43)")
  (second (read s)))
(with-input-from-string (s "(41 42 43)"
                           :start 4
                           :end 5)
  (read s))
(with-output-to-string (s)
  (write-string "Look: " s)
  (princ (list 1 2 3) s))
(let ((string (make-array 0 :element-type 'character
                            :fill-pointer t
                            :adjustable t)))
  (vector-push-extend #\[ string)
  (with-output-to-string (s string)
    (write-string "Look: " s)
    (princ (list 1 2 3) s))
  (vector-push-extend #\] string)
  string)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(loop with ptr = 0
      with eof = (gensym)
      for from = ptr
      for object = (with-input-from-string
                       (s "41  42 43"
                          :start ptr
                          :index ptr)
                     (read s nil eof))
      until (eq object eof)
      do (format t "~A-~A: ~A~%" from ptr object)
      finally (return (values)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(let* ((s (make-string-input-stream "41 42 43"))
       (a (read s))
       (b (read s)))
  (list a b (read s)))
(let ((s (make-string-output-stream)))
  (write-string "Look: " s)
  (princ (list 1 2 3) s)
  (format t "1: ~S~%" (get-output-stream-string s))
  (write-string "More..." s)
  (format t "2: ~S~%" (get-output-stream-string s)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(read-line
 (make-concatenated-stream
  (make-string-input-stream "Duc")
  (make-string-input-stream "k So")
  (make-string-input-stream "up")))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(with-open-file (out "/tmp/foo.txt"
                     :direction :output)
  (write-line "First line" out)
  (write-line "Second line" out)
  (write-line "The third line" out :start 4 :end 9)
  (write-string "Last line, without Newline" out))
(with-open-file (in "/tmp/foo.txt")
  (loop for (line no-nl-p)
          = (multiple-value-list (read-line in nil nil))
        while line
        do (format t "~S~:[ <newline at end>~;~]~%"
                   line no-nl-p)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(with-open-file (out "/tmp/foo.txt"
                     :direction :output
                     :if-exists :supersede
                     :external-format
                     '(:e-crlf :latin1))
  (write-line "First line" out)
  (write-line "Second line" out)
  (write-line "The third line" out
              :start 4 :end 9)
  (write-string "Last line, without Newline" out))
(with-open-file (in "/tmp/foo.txt")
  (loop for line = (read-line in nil)
        while line
        do (format t "~S~%"
                   (char line
                         (1- (length line))))))
(with-open-file (in "/tmp/foo.txt"
                    :external-format
                    '(:e-crlf :latin1))
  (loop for line = (read-line in nil)
        while line
        do (format t "~S~%"
                   (char line
                         (1- (length line))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(handler-case
  (with-open-file (in "/tmp/foo.txt")
    (loop for line = (read-line in)
          do (format t "~S~%" line)))
  (end-of-file ()
    (format t "-->END OF FILE HERE<--~%")))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(with-open-file (out "/tmp/data"
                     :direction :output
                     :element-type
                     '(signed-byte 16))
  (dolist (byte '(28483 27503 28514 27503))
    (write-byte byte out)))
(with-open-file (in "/tmp/data"
                    :element-type
                    '(signed-byte 16))
  (loop for byte = (read-byte in nil)
        while byte
        collect byte))
(with-open-file (in "/tmp/data"
                    :element-type
                    '(signed-byte 32))
  (loop for byte = (read-byte in nil)
        while byte
        collect byte))
(+ 28483 (* 65536 27503))
(with-open-file (in "/tmp/data"
                    :element-type
                    '(unsigned-byte 8))
  (loop for byte = (read-byte in nil)
        while byte
        collect byte))
(+ 67 (* 256 111))
(with-open-file (in "/tmp/data")
  (read-line in))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(with-open-file (out "/tmp/data"
                     :direction :output
                     :if-exists :supersede
                     :element-type
                     '(signed-byte 16))
  (write-sequence '(100 200 300 400 500 600 700)
                  out :start 2 :end 6))
(with-open-file (in "/tmp/data"
                    :element-type
                    '(signed-byte 16))
  (let ((result (make-array 8 :initial-element 0)))
    (read-sequence result in :start 3 :end 7)
    result))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(with-open-file (out "/tmp/data"
                     :direction :output
                     :if-exists :supersede
                     :element-type '(mod 16))
  (dotimes (i 16)
    (write-byte i out)))
(let ((*print-length* nil))
  (with-open-file (in "/tmp/data"
                      :element-type '(mod 16))
    (print (loop for byte = (read-byte in nil)
                 while byte
                 collect byte)))
  (values))
(let ((*print-length* nil))
  (with-open-file (in "/tmp/data"
                      :element-type
                      '(unsigned-byte 8))
    (print (loop for byte = (read-byte in nil)
                 while byte
                 collect byte)))
  (values))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; some Lisps will signal an error, some won't:
(with-open-file (out "/tmp/data"
                     :direction :output
                     :if-exists :supersede
                     :element-type '(mod 16))
  (write-byte 16 out))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; see C program "data.c" in this directory
(rename-package :binary-types :bt)
(bt:define-binary-class data-point ()
  ((tag
    :accessor tag
    :binary-type bt:char8)
   ;; see remark about padding above
   (padding-1
    :binary-type 1)
   (x0
    :accessor x0
    :binary-type bt:u16)
   (y0
    :accessor y0
    :binary-type bt:u16)
   ;; see above
   (padding-2
    :binary-type 2)
   (size
    :accessor size
    :binary-type bt:s32)
   ;; so that size is 16 octets
   (padding-3
    :binary-type 4)))
(defmethod print-object ((data-point data-point)
                         stream)
  (print-unreadable-object
      (data-point stream :type t)
    (with-accessors ((tag tag)
                     (x0 x0)
                     (y0 y0)
                     (size size))
        data-point
      (format stream "Tag: ~S, " tag)
      (format stream "X0: ~S, " x0)
      (format stream "Y0: ~S, " y0)
      (format stream "Size: ~S" size))))
(let ((bt:*endian* :little-endian))
  (bt:with-binary-file (in "/tmp/data")
    (values (bt:read-binary 'data-point in)
            (bt:read-binary 'data-point in))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; see C program "fdata.c" in this directory
(with-open-file (in "/tmp/data" :element-type '(unsigned-byte 64))
  (loop repeat 2
        collect (ieee-floats:decode-float64 (read-byte in))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(with-open-file (out "/tmp/data"
                     :direction :output
                     :element-type '(signed-byte 32))
  (loop for i below 1000
        do (write-byte (* i i) out)))
(with-open-file (in "/tmp/data"
                    :element-type '(signed-byte 32))
  (file-position in 333)
  (values (read-byte in)
          (file-position in)))
(* 333 333)
(with-open-file (out "/tmp/data.txt"
                     :direction :output)
  (write-string
   "Now, fair Hippolyta, our nuptial hour
Draws on apace; four happy days bring in
Another moon: but, O, methinks, how slow
This old moon wanes! she lingers my desires,
Like to a step-dame or a dowager
Long withering out a young man revenue." out))
"Now, fair Hippolyta, our nuptial hour
Draws on apace; four happy days bring in
Another moon: but, O, methinks, how slow
This old moon wanes! she lingers my desires,
Like to a step-dame or a dowager
Long withering out a young man revenue."
(with-open-file (in "/tmp/data.txt")
  (file-position in 190)
  (let ((string (make-string 7)))
    (read-sequence string in)
    (values string
            (file-position in))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(with-open-file (out "/tmp/data.txt"
                     :direction :output
                     :if-exists :supersede)
  (write-string
   "The present day sysadmin refuses to die." out))
(file-at-once "/tmp/data.txt")
(with-open-file (out "/tmp/data.txt"
                     :direction :output
                     :if-exists :overwrite)
  (file-position out 16)
  (write-string "composer" out)
  (file-position out :end)
  (write-string " (Edgar Varese)" out))
(file-at-once "/tmp/data.txt")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun file-position* (stream &optional (pos nil pos-provided-p))
  (cond ((not pos-provided-p)
         (file-position stream))
        ((and (integerp pos) (< pos 0))
         (file-position stream (+ (file-length stream) pos)))
        (t (file-position stream pos))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; external format is specific to AllegroCL
(with-open-file (in "/tmp/test.txt"
                    :external-format
                    '(:e-crlf :latin1))
  (loop for line = (read-line in nil)
        while line
        collect line))
(with-open-file (in "/tmp/test.txt"
                    :external-format
                    '(:e-crlf :latin1))
  (let ((line (read-line in)))
    (values (length line)
            (file-position in))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(with-open-file (out "/tmp/store"
                     :direction :output)
  (print #\x out)
  (print 42 out)
  (print 'foo out)
  (print (* 2 pi) out)
  (print "Zoot Allures" out)
  (print (loop for i below 10 collect i) out))
(with-open-file (in "/tmp/store")
  (loop with eof = (gensym)
        for object = (read in nil eof)
        until (eq object eof)
        collect object))
(file-at-once "/tmp/store")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun circle-test ()
  (let* ((a (list 1 2 3))
         (b (cons 0 a)))
    (with-open-file (out "/tmp/store"
                         :direction :output
                         :if-exists :supersede)
      (print (list a b) out))
    (with-open-file (in "/tmp/store")
      (let* ((c (read in))
             (a% (first c))
             (b% (second c)))
        (format t "Read ~S and ~S~%" a% b%)
        (values (eq a (cdr b))
                (eq a% (cdr b%)))))))
(circle-test)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; continued from above
(let ((*print-circle* t))
  (circle-test))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; not all implementations will signal an error here
(let ((*print-readably* t))
  (print (make-hash-table)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun print-eql-hash-table (hash-table
                             &optional (stream *standard-output*))
  (format stream "#.")
  (pprint `(let ((new-hash-table (make-hash-table)))
             ,@(loop for key being the hash-keys of hash-table
                     using (hash-value value)
                     collect `(setf (gethash ',key new-hash-table)
                                    ',value))
             new-hash-table)
          stream))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defparameter *h* (make-hash-table))
(setf (gethash 13 *h*) 42)
(setf (gethash 'foo *h*) 'bar)
(print-equal-hash-table *h*)
(defparameter *h2* (read-from-string
                    (with-output-to-string (out)
                      (print-equal-hash-table *h* out))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(ql:quickload "trivial-gray-streams")
(use-package :trivial-gray-streams)
(defclass my-vector-stream (fundamental-binary-input-stream)
  ((vector :initarg :vector)
   (index :initform 0)))
(defmethod stream-read-byte ((stream my-vector-stream))
  (with-slots (index vector) stream
    ;; return one byte or the keyword :EOF
    (cond ((< index (length vector))
           (prog1 (aref vector index)
             ;; move "position within stream"
             (incf index)))
          (t :eof))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defparameter *in*
  (make-instance 'my-vector-stream
                 :vector #(42 43 44 45 46 47)))
(list
 (read-byte *in*)
 (let ((list (make-list 3)))
   (read-sequence list *in*)
   list)
 (read-byte *in*)
 (streamp *in*)
 (open-stream-p *in*))
(progn
  (read-byte *in*)
  (read-byte *in*))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass my-dupe-stream
          (fundamental-character-output-stream)
  ())
(defmethod stream-write-char ((stream my-dupe-stream) char)
  (write-char char)
  (unless (char-equal char #\Newline)
    (write-char char)))
(defparameter *out* (make-instance 'my-dupe-stream))
(print 42 *out*)
(format *out* "~R~%" 42)
(write-string "cooeeing" *out*)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
