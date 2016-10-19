;;; Copyright (c) 2015, Edmund Weitz.  All rights reserved.

;;; This is example code for the book "Common Lisp Recipes" and meant
;;; to be used with something like (from SLIME) C-M-x or C-c C-c.
;;; See the book for more information.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (error "This code is not meant to be used with LOAD or COMPILE-FILE."))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(char-code #\a)
(char-code #\A)
(char-code #\ü)
(char-code #\א)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(code-char 97)
(code-char 65)
(code-char 252)
(code-char 1488)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(char-name #\A)
(char-name #\a)
(name-char "Latin_Small_Letter_A")
#\latin_small_letter_a
(char-name (code-char 1488))
#\HEBREW_LETTER_ALEF
#\U+05D0
(name-char "U+05D0")
(name-char "A")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(with-open-file (out "/tmp/foo.txt"
                     :direction :output
                     :if-exists :supersede
                     :element-type '(unsigned-byte 8))
  (write-byte 195 out)
  (write-byte 156 out))
(with-open-file (out "/tmp/foo.txt"
                     :direction :output
                     :if-exists :append
                     :element-type 'character
                     :external-format :latin-1)
  (write-string "berjazz" out))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(with-open-file (in "/tmp/foo.txt"
                    :element-type 'character
                    :external-format :utf-8)
  (read-line in))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(char= #\a #\a)
(char= #\a #\b)
(char= #\a #\A)
(char-equal #\a #\A)
(char< #\a #\b)
(char< #\A #\b)
(char< #\a #\B)
(char-lessp #\A #\b)
(char-lessp #\a #\B)
(eql "foo" "foo")
(string= "foo" "foo")
(equal "foo" "foo")
(string= "foo" "Foo")
(equal "foo" "Foo")
(string-equal "foo" "Foo")
(equalp "foo" "Foo")
(string< "adam" "eve")
(string< "aardvark" "aardwolf")
(string< "werewolf" "aardwolf")
(string< "aardvark" "Aardwolf")
(string-lessp "aardvark" "Aardwolf")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(both-case-p (code-char #x17F))
(char-equal #\S (code-char #x17F))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(format nil "~Cberjazz" #\U+00DC)
(let ((foo "Ü"))
  (format nil "~Aberjazz" foo))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(format nil "~Cberjazz" #\U+00DC)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#.(format nil "~Cberjazz" #\U+00DC)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(char-upcase #\a)
(char-downcase #\A)
(char-downcase #\a)
(char-downcase #\Space)
(char-downcase #\greek_capital_letter_alpha)
(upper-case-p #\A)
(lower-case-p #\a)
(upper-case-p #\Space)
(lower-case-p #\Space)
(both-case-p #\Space)
(both-case-p #\hebrew_letter_alef)
(string-upcase "miles davis")
(string-downcase "MILES")
(string-capitalize "miles DAVIS")
(string-upcase "miles davis" :start 0 :end 6)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(let ((string (copy-seq "Grover Washington, jr.")))
  (setf (char string 19)
        (char-upcase (char string 19)))
  string)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(format nil "Downcase: ~(~A~)" "FOO")
(format nil "Capitalize: ~:(~A~)" "FOO BAR BAZ")
(format nil "Capitalize first word, downcase rest: ~@(~A~)"
            "FOO BAR BAZ")
(format nil "Upcase: ~:@(~A~)" "Foo BAR baz")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(subseq "Cookbook" 4)
(subseq "Cookbook" 4 7)
(let ((string1 (copy-seq "Harpo Marx"))
      (string2 (copy-seq "Groucho, Harpo, and Chico")))
  (setf (subseq string1 0 5) "Zeppo")
  (print string1)
  (setf (subseq string1 0 5) "Groucho")
  (print string1)
  (setf string1
        (replace string1 string2
                 :start1 0 :end1 5
                 :start2 9 :end2 14))
  (print string1)
  (setf (subseq string1 0) "Groucho")
  string1)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(let* ((string1 (copy-seq "walk"))
       (string2 (subseq string1 0)))
  (setf (char string2 0) #\t)
  (values string1 string2))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(find #\o "We're Only In It For The Money")
(find #\o "We're Only In It For The Money"
      :test 'char-equal)
(position #\o "We're Only In It For The Money")
(position #\O "We're Only In It For The Money")
(search "on" "We're Only In It For The Money")
(search "on" "We're Only In It For The Money"
        :test 'char-equal)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(string-trim '(#\Space #\Linefeed) "
    This is a sentence.  ")
(string-left-trim "([" "([foo])")
(string-right-trim ")]" *)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(let* ((string1 (copy-seq "abc"))
       (string2 (string-trim "x" string1)))
  (setf (char string2 0) #\A)
  (list string1 string2))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; assumes something like ASCII - see footnote
;; see https://en.wikipedia.org/wiki/Digital_root
(defun digital-root (string)
  (assert (every #'digit-char-p string)
          (string)
          "~S doesn't denote a non-negative decimal integer."
          string)
  (loop for char across string
        sum (digit-char-p char) into result
        finally (return
                  (if (> result 9)
                      (digital-root (princ-to-string result))
                      result))))
(digital-root "12")
(digital-root "1234")
;; assumes something like ASCII - see footnote
;; see https://en.wikipedia.org/wiki/ROT13
(defun rot13-char (char)
  (cond ((char<= #\a char #\z)
         (code-char (+ (mod (+ (- (char-code char) (char-code #\a))
                               13)
                            26)
                       (char-code #\a))))
        ((char<= #\A char #\Z)
         (code-char (+ (mod (+ (- (char-code char) (char-code #\A))
                               13)
                            26)
                       (char-code #\A))))))
(map 'string #'rot13-char "foobar")
(map 'string #'rot13-char *)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(let ((string "frob"))
  (values (aref string 0)
          (char string 1)
          (schar string 2)
          (subseq string 3 4)))
(let ((string "baz"))
  (loop for i below (length string)
        collect (char string i)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(coerce "Recipes" 'list)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun n-rot13-string (string)
  (loop for i below (length string)
        do (setf (char string i)
                 (rot13-char (char string i)))))
(defparameter *string* (copy-seq "foobar"))
(n-rot13-string *string*)
*string*
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; or like this:
(defun n-rot13-string (string)
  (map-into string 'rot13-char string))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun join (separator list)
  (with-output-to-string (out)
    (loop for (element . more) on list
          do (princ element out)
          when more
            do (princ separator out))))
(join #\Space '("This" "is" "it"))
(join #\- '(2003 12 31))
(join ", " '("C"  "C++" "C#"))
(join "" '("Hallo" "ween"))
(join #\- '())
(join #\- '("One item only"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun join (separator list)
  (with-output-to-string (out)
    (loop (princ (or (pop list) "") out)
          (unless list (return))
          (princ separator out))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun join (separator list)
  (with-output-to-string (out)
    (when list
      (princ (pop list) out))
    (loop (unless list (return))
       (princ separator out)
       (princ (pop list) out))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(format nil "~{~A~^, ~}" (list "C" "C++" "C#"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun list-join (separator list)
  (loop for (element . more) on list
        collect element
        when more
          collect separator))
(list-join '+ (loop for i below 5 collect i))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defparameter *csv-readtable* (copy-readtable))
(set-syntax-from-char #\, #\Space *csv-readtable*)
(defun read-csv-line (string)
  (let ((*readtable* *csv-readtable*))
    (with-input-from-string (stream string)
      (loop for object = (read stream nil nil)
            while object
            collect object))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; see separate file "test.tsv" in this directory
(with-open-file (stream "/tmp/test.csv")
  (loop for line = (read-line stream nil nil)
        while line
        collect (read-csv-line line)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
