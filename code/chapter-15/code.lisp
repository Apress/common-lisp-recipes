;;; Copyright (c) 2015, Edmund Weitz.  All rights reserved.

;;; This is example code for the book "Common Lisp Recipes" and meant
;;; to be used with something like (from SLIME) C-M-x or C-c C-c.
;;; See the book for more information.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (error "This code is not meant to be used with LOAD or COMPILE-FILE."))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
*default-pathname-defaults*

(probe-file "passwd")
(let ((*default-pathname-defaults* #p"/etc/"))
  (probe-file "passwd"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(with-open-file (out "/tmp/foo.txt"
                     :direction :output
                     :if-exists :supersede)
  (write-string "42" out))
(probe-file #p"foo")
(let ((*default-pathname-defaults* #p"/tmp/whatever.txt"))
  (probe-file #p"foo"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; continued from above
(pathname-type #p"foo")
(let ((*default-pathname-defaults* #p"/tmp/whatever.txt"))
  (probe-file (make-pathname :name "foo"
                             :type :unspecific)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(probe-file "/tmp/foo")
(with-open-file (s "/tmp/foo" :direction :output)
  (write-string "bla" s))
(probe-file "/tmp/foo")
(ensure-directories-exist
  (make-pathname :directory
                 '(:absolute "tmp" "bar")))
(probe-file *)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; continued from above
(directory "/tmp/foo*")
(with-open-file (s "/tmp/foo2" :direction :output)
  (write-string "bla" s))
(directory "/tmp/foo*")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(ensure-directories-exist "/tmp/foo/bar/")
(ensure-directories-exist "/tmp/foo/bar/baz/frob"
                          :verbose t)
(ensure-directories-exist "/tmp/foo/bar/baz/frob"
                          :verbose t)
(ensure-directories-exist "/tmp/foo/bar/baz/frob/"
                          :verbose t)
;; this will only work on AllegroCL
(excl:make-directory "/tmp/bar" #o700)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(ensure-directories-exist (make-pathname :directory
                                         '(:absolute "tmp"
                                                     "foo"
                                                     "bar"
                                                     "baz"
                                                     "frob")
                                         :name "dummy"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(directory (make-pathname :name :wild :type :wild
                          :directory '(:absolute "tmp" "foo")))
(directory (make-pathname :name :wild :type "lisp"
                          :directory '(:absolute "tmp" "foo")))
(directory (make-pathname :name "a" :type :wild
                          :directory '(:absolute "tmp" "foo")))
(directory "/tmp/foo/*.lisp")
(directory "/tmp/foo/a.*")
(directory "/tmp/foo/*")
(directory "/tmp/foo/*.*")
(directory "/tmp/foo/ba*.lisp")
(directory (make-pathname :name "ba*" :type :wild
                          :directory "/tmp/foo/"))
(directory (make-pathname :name "ba?" :type :wild
                          :directory "/tmp/foo/"))
(directory (make-pathname :name "*b*" :type :wild
                          :directory "/tmp/foo/"))
(directory "/tmp/foo/b*.lisp")
(directory "/tmp/foo/b*.*")
(directory "/tmp/foo/b??.lisp")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; continued from above
(directory
 (make-pathname :directory
                '(:absolute "tmp" "foo" :wild "baz")
                :name :wild :type :wild))
(directory
 (make-pathname :directory
                '(:absolute "tmp" "foo" :wild "baz")
                :name "frob*" :type :wild))
(directory
 (make-pathname :directory
                '(:absolute "tmp" "foo" :wild :wild)
                :name "frob*" :type :wild))
(directory "/tmp/foo/*/baz/*")
(directory "/tmp/foo/*/baz/frob*")
(directory "/tmp/foo/*/*/frob*")
(directory "/tmp/foo/bar*/baz/frob*")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; continued from above
(directory
 (make-pathname :directory
                '(:absolute "tmp" "foo" :wild-inferiors)
                :name "frob*" :type :wild))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun split-pathspec (pathspec)
  (values (directory-namestring pathspec)
          (file-namestring pathspec)))
(split-pathspec #p"/etc/passwd")
(split-pathspec #p"/usr/local/lib/")
(split-pathspec #p"/usr/lib/libc.so")
(probe-file #p"foo.doc")
(split-pathspec #p"foo.doc")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(pathname-type #p"libc.so")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(pathname-type #p"foo.tar.gz")
(pathname-type #p".bashrc")
(pathname-type #p"foo.")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(ensure-directories-exist "/tmp/foo/dummy.txt")
(directory "/tmp/foo/*.*")
(with-open-file (s "/tmp/foo/a.txt"
                   :direction :output))
(directory "/tmp/foo/*.*")
(rename-file "/tmp/foo/a.txt" "/tmp/foo/b.txt")
(directory "/tmp/foo/*.*")
(rename-file "/tmp/foo/b.txt" "c.txt")
(directory "/tmp/foo/*.*")
(rename-file "/tmp/foo/c.txt" "d")
(directory "/tmp/foo/*.*")
(rename-file "/tmp/foo/d.txt"
             (make-pathname :type "lisp"))
(directory "/tmp/foo/*.*")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(directory "/tmp/foo/*.*")
(excl.osi:rename "/tmp/foo/a.txt" "/tmp/foo/b")
(directory "/tmp/foo/*.*")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(rename-file "foo/a.txt" "bar/b.txt")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(probe-file "foo.lisp")
(delete-file "foo.lisp")
(probe-file "foo.lisp")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; this example will only work with AllegroCL
(defun delete-files (pathspec)
  (dolist (pathname (directory pathspec))
    (unless (excl:file-directory-p pathname)
      (delete-file pathname))))
(directory #p"bar*" :directories-are-files nil)
(delete-files #p"bar*")
(directory #p"bar*" :directories-are-files nil)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(probe-file #p"foo.txt")
(with-open-file (out #p"foo.txt" :direction :output)
  (values (probe-file #p"foo.txt")
          (probe-file out)
          (progn (delete-file out)
                 (probe-file #p"foo.txt"))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; example for AllegroCL
(directory "/tmp/foo/*.*")
(excl:delete-directory "/tmp/foo")
(directory "/tmp/bar/*.*")
(excl:delete-directory "/tmp/bar")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun copy-file (from to)
  (let* ((element-type '(unsigned-byte 8))
         (buffer (make-array 8192 :element-type element-type)))
    (with-open-file (in from :element-type element-type)
      (with-open-file (out to :element-type element-type
                              :direction :output
                              :if-exists :supersede)
        (loop (let ((position (read-sequence buffer in)))
                (when (zerop position)
                  (return))
                (write-sequence buffer out :end position)))
        (pathname out)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(let ((tex-counter 0)
      (sty-counter 0))
  (fad:walk-directory "C:/Users/edi/Documents/MiKTeX/"
                      (lambda (pathname)
                        (let ((type (pathname-type pathname)))
                          (cond ((string-equal type "tex")
                                 (incf tex-counter))
                                ((string-equal type "sty")
                                 (incf sty-counter))))))
  (list tex-counter sty-counter))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(let ((tex-counter 0))
  (fad:walk-directory "C:/Users/edi/Documents/MiKTeX"
                      (lambda (pathname)
                        (declare (ignore pathname))
                        (incf tex-counter))
                      :test (lambda (pathname)
                              (string-equal (pathname-type pathname)
                                            "tex")))
  tex-counter)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(with-open-file (in "/etc/passwd")
  (pathname in))
(let ((in (open "/etc/passwd")))
  (close in)
  (pathname in))
(pathname *standard-output*)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(let ((pathname (pathname "/etc/passwd")))
  (with-open-file (in pathname)
    (eq pathname (pathname in))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; see shell script "prepare.sh" in this directory
(pathname "/tmp/foo.txt")
(pathname "/tmp/bar.txt")
(truename "/tmp/bar.txt")
(probe-file "/tmp/quux.txt")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; continued from above
(directory "/tmp/*.txt")
(directory "/tmp/*.txt" :resolve-symlinks nil)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; see shell script "prepare2.sh" in this directory
(truename "/tmp/quux/..")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(truename
 (make-pathname :directory
                (list :absolute "tmp" "quux" :up)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(truename
 (make-pathname :directory
                (list :absolute "tmp" "quux" :back)))  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; see file "foo.lisp" in this directory
(load (compile-file "foo.lisp"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; make modifications in "foo.lisp", then again...
(load (compile-file "foo.lisp"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setf (logical-pathname-translations "STUFF")
      '(("SOURCE;**;*.*" "D:\\Dev\\**\\*.*")
        ("RESOURCES;**;*.JPG" "\\\\data.quux.com\\**\\pics\\*.jpg")
        ("RESOURCES;**;*.*" "\\\\data.quux.com\\**\\*.*")))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setf (logical-pathname-translations "STUFF")
      '(("SOURCE;**;*.*" "/usr/local/lisp/**/*.*")
        ("RESOURCES;**;BACKUP;**;*.*" "/mnt/bak/**/**/*.*")
        ("RESOURCES;**;*.*" "~/data/**/*.*")))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
