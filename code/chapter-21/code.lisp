;;; Copyright (c) 2015, Edmund Weitz.  All rights reserved.

;;; This is example code for the book "Common Lisp Recipes" and meant
;;; to be used with something like (from SLIME) C-M-x or C-c C-c.
;;; See the book for more information.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (error "This code is not meant to be used with LOAD or COMPILE-FILE."))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass quux () ((a :initarg :a :reader a)))
(defpackage frob)
(defparameter *thing*
  (let* ((list (list :foo))
         (hash (make-hash-table)))
    (setf (gethash 42 hash) list)
    (vector #\x "x" (make-instance 'quux :a 42)
            (intern "X" :frob) list hash)))
(cl-store:store *thing* "/tmp/store")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; continued from above
(defparameter *other-thing*
  (cl-store:restore "/tmp/store"))
*other-thing*
(eq (aref *other-thing* 4)
    (gethash 42 (aref *other-thing* 5)))
(a (aref *other-thing* 2))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(cl-store:restore "/tmp/store")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defparameter *thing*
  (let* ((list (list :foo))
         (hash (make-hash-table)))
    (setf (gethash 42 hash) list)
    (vector (find-package :frob) (find-class 'quux)      ;; <- added
            #\x "x" (make-instance 'quux :a 42)
            (intern "X" :frob) list hash)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(clsql:connect '("/tmp/worldcup.db") :database-type :sqlite3)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(clsql:def-view-class final ()
  ((city :accessor city
         :initarg :city
         :type string)
   (year :accessor year
         :initarg :year
         :db-kind :key
         :type integer)
   (winner :accessor winner
           :initarg :winner
           :type keyword)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(clsql:create-view-from-class 'final)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(make-instance 'final :city "Rome" :year 1934 :winner :ita)
(clsql:update-records-from-instance *)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(let ((clsql:*db-auto-sync* t))
  (loop for (city year winner) in '(("Paris" 1938 :ita)
                                    ("Bern" 1954 :deu)
                                    ("Solna" 1958 :bra)
                                    ("Santiago" 1962 :bra)
                                    ("Mexico City" 1970 :bra)
                                    ("Munich" 1974 :deu)
                                    ("Madrid" 1982 :ita)
                                    ("Rome" 1990 :deu)
                                    ("Pasadena" 1994 :bra)
                                    ("Yokohama" 2002 :bra)
                                    ("Berlin" 2006 :ita)
                                    ("Rio" 2014 :deu))
        for final = (make-instance 'final :city city
                                          :year year
                                          :winner winner)
        finally (return final)))
;; oh wait, we made a mistake in the last object; let's fix it:
(let ((clsql:*db-auto-sync* t))
  (setf (city *) "Rio de Janeiro"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(clsql:select 'final :flatp t)
(describe (first (last *)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; this is for the [...] syntax used below
(clsql:enable-sql-reader-syntax)
(clsql:select 'final :where [= [city] "Rome"] :flatp t)
(mapcar 'winner *)

;; now we lose the CLOS objects and read the data "directly"
(clsql:select [winner] [count [*]]
              :from [final] :group-by [winner])

;; you can also transmit SQL statements as strings if you prefer
(clsql:query "select distinct winner from final")

;; various looping constructs are available
(clsql:do-query ((winner) [select [distinct [winner]]
                                  :from [final]])
  (princ winner))

;; and even a modified LOOP (which will work in /some/ Lisps)
(loop for winner being the records
          of [select [winner] :from [final]]
      count (string= ":ITA" winner))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass final (bknr.datastore:store-object)
  ((city :accessor city
         :initarg :city)
   (year :accessor year
         :initarg :year)
   (winner :accessor winner
           :initarg :winner))
  (:metaclass bknr.datastore:persistent-class))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(let ((object-subsystem
       (make-instance 'bknr.datastore:store-object-subsystem)))
  (make-instance 'bknr.datastore:mp-store
                 :directory "/tmp/store/"
                 :subsystems (list object-subsystem)))
bknr.datastore:*store*
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(loop for (city year winner) in '(("Rome" 1934 :ita)
                                  ("Paris" 1938 :ita)
                                  ("Bern" 1954 :deu)
                                  ("Stockholm" 1958 :bra))
      for final = (make-instance 'final :city city
                                        :year year
                                        :winner winner)
      finally (return final))
;; Oops, let's fix that...
(bknr.datastore:with-transaction ()
  (setf (city *) "Solna"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(bknr.datastore:store-objects-with-class 'final)
(describe (first (last *)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass final (bknr.datastore:store-object)
  ((city :accessor city
         :initarg :city)
   (year :accessor year
         :initarg :year
         :index-type bknr.indices:unique-index        ;; added
         :index-reader final-by-year                  ;; added
         :index-values all-finals)                    ;; added
   (winner :accessor winner
           :initarg :winner))
  (:metaclass bknr.datastore:persistent-class))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(final-by-year 1934)
(length (all-finals))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require :acache)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass final ()
  ((city :accessor city
         :initarg :city)
   (year :accessor year
         :initarg :year
         :index :any-unique)
   (winner :accessor winner
           :initarg :winner))
  (:metaclass db.ac:persistent-class))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(db.ac:open-file-database "/tmp/db/"
                          :if-does-not-exist :create
                          :if-exists :supersede)
db.ac:*allegrocache*
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(loop for (city year winner) in '(("Rome" 1934 :ita)
                                  ("Paris" 1938 :ita)
                                  ("Bern" 1954 :deu)
                                  ("Stockholm" 1958 :bra))
      for final = (make-instance 'final :city city
                                        :year year
                                        :winner winner)
      finally (return final))
;; Oops, the same error again...
(setf (city *) "Solna")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(db.ac:commit)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(db.ac:open-file-database "/tmp/db/")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(db.ac:retrieve-from-index 'final 'year 1958)
(describe *)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
