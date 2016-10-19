;;; Copyright (c) 2015, Edmund Weitz.  All rights reserved.

;;; This is example code for the book "Common Lisp Recipes" and meant
;;; to be used with something like (from SLIME) C-M-x or C-c C-c.
;;; See the book for more information.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (error "This code is not meant to be used with LOAD or COMPILE-FILE."))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defparameter *h* (make-hash-table))
(gethash 'batman *h*)
(setf (gethash 'batman *h*) 'gotham-city)
(gethash 'batman *h*)
(setf (gethash 'superman *h*) 'duckburg)
(gethash 'superman *h*)
(setf (gethash 'superman *h*) 'metropolis)
(gethash 'superman *h*)
(gethash 'spider-man *h*)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setf (gethash 'lois-lane *h*) 'metropolis)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defparameter *h* (make-hash-table))
(gethash 'batman *h*)
(setf (gethash 'batman *h*) nil)
(gethash 'batman *h*)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defparameter *h* (make-hash-table))
(setf (gethash 'gladstone-gander *h*) 'goose)
(setf (gethash 'gyro-gearloose *h*) 'chicken)
(defun duckburg-species (name)
  (gethash name *h*))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(duckburg-species 'gyro-gearloose)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun duckburg-species (name)
  (gethash name *h* 'duck))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(duckburg-species 'gyro-gearloose)
(duckburg-species 'donald-duck)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun duckburg-species (name)
  (or (gethash name *h*) 'duck))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defparameter *h* (make-hash-table))
(setf (gethash 'batman *h*) 'gotham-city)
(setf (gethash 'superman *h*) 'metropolis)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setf (gethash 'superman *h*) nil)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(remhash 'superman *h*)
(gethash 'superman *h*)
(hash-table-count *h*)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defparameter *h* (make-hash-table))
(loop for (key value) in '((superman 1938)
                           (donald-duck 1934)
                           (batman 1939)) do
      (setf (gethash key *h*) value))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(let ((min 2015) oldest)
  (maphash (lambda (hero year)
             (when (< year min)
               (setf min year
                     oldest hero)))
           *h*)
  oldest)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(let ((min 2015) oldest)
  (with-hash-table-iterator (next-hero *h*)
    (loop
     (multiple-value-bind (not-done hero year)
         (next-hero)
       (unless not-done
         (return oldest))
       (when (< year min)
         (setf min year
               oldest hero))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun my-maphash (function hash-table)
  (with-hash-table-iterator (next-entry hash-table)
    (loop (multiple-value-bind (more key value)
              (next-entry)
            (unless more (return nil))
            (funcall function key value)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun hero-from (this-year)
  (with-hash-table-iterator (next-hero *h*)
    (loop
     (multiple-value-bind (not-done hero year)
         (next-hero)
       (unless not-done
         (return nil))
       (when (= year this-year)
         ;; skip the rest, we're done
         (return hero))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(loop with min = 2015 and oldest
      for hero being the hash-keys of *h*
        using (hash-value year)
      when (< year min)
        do (setf min year
                 oldest hero)
      finally (return oldest))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(loop with min = 2015 and oldest
      for year being the hash-values of *h*
        using (hash-key hero)
      when (< year min)
        do (setf min year
                 oldest hero)
      finally (return oldest))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(loop for hero being the hash-keys of *h*
      collect hero)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(loop for hero being the hash-keys of *h*
        using (hash-value year)
      when (< year 1935)
        do (remhash hero *h*))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; don't do that!
(loop for hero being the hash-keys of *h*
        using (hash-value year)
      when (eql hero 'batman)
        do (setf (gethash 'robin *h*) (1+ year)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro dohash ((key-name value-name hash-table) &body body)
  (let ((next (gensym "NEXT"))
        (more (gensym "MORE")))
    `(with-hash-table-iterator (,next ,hash-table)
       (loop (multiple-value-bind (,more ,key-name ,value-name)
                 (,next)
               (unless ,more (return nil))
               ,@body)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(dohash (hero year *h*)
  (format t "~A: ~A~%" year hero))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(iter (for (hero year) in-hashtable *h*)
      (format t "~A: ~A~%" year hero))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defparameter *h* (make-hash-table))
(setf (gethash "Batman" *h*) "Gotham City")
(gethash "Batman" *h*)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defparameter *h* (make-hash-table :test 'equal))
(setf (gethash "Batman" *h*) "Gotham City")
(gethash "Batman" *h*)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defparameter *h* (make-hash-table :test 'equal))
(loop for (key value) in '(("Superman" 1938)
                           ("Donald Duck" 1934)
                           ("Batman" 1939)) do
      (setf (gethash key *h*) value))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setf (gethash "Daisy Duck" *h*) 1940)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defparameter *h* (make-hash-table))
(hash-table-count *h*)
(time (loop for n below 1000000 do (setf (gethash n *h*) n)))
(hash-table-count *h*)
(clrhash *h*)
(hash-table-count *h*)
(time (loop for n below 1000000 do (setf (gethash n *h*) n)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defparameter *h* (make-hash-table))
(hash-table-count *h*)
(hash-table-size *h*)
(hash-table-rehash-size *h*)
(hash-table-rehash-threshold *h*)
(time (loop for n below 1000000 do (setf (gethash n *h*) n)))
(hash-table-count *h*)
(hash-table-size *h*)
(clrhash *h*)
(hash-table-count *h*)
(hash-table-size *h*)
(time (loop for n below 1000000 do (setf (gethash n *h*) n)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defparameter *h* (make-hash-table :size 1000000))
(time (loop for n below 1000000 do (setf (gethash n *h*) n)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass worker ()
  ((id :initarg :id)))

(defparameter *workers* ())

(defparameter *buffer-hash* (make-hash-table))

(defun add-worker (id &optional with-buffer-p)
  (let ((new-worker (make-instance 'worker :id id)))
    (push new-worker *workers*)
    (when with-buffer-p
      (setf (gethash new-worker *buffer-hash*)
            (make-array 1024)))
    new-worker))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(dotimes (i 10)
  (add-worker i (oddp i)))
(list (length *workers*)
      (hash-table-count *buffer-hash*))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(pop *workers*)
(list (length *workers*)
      (hash-table-count *buffer-hash*))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defparameter *buffer-hash* (make-hash-table :weak-kind :key))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(list (length *workers*)
      (hash-table-count *buffer-hash*))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defparameter *a* (list (cons 'superman 'metropolis)
                        (cons 'batman 'gotham-city)))
(assoc 'batman *a*)
(cdr (assoc 'batman *a*))
(assoc 'donald-duck *a*)
(push (cons 'donald-duck 'duckburg) *a*)
(assoc 'donald-duck *a*)
(push (cons 'donald-duck 'entenhausen) *a*)
(assoc 'donald-duck *a*)
(progn (pop *a*) (pop *a*) *a*)
(setf *a* (acons 'donald-duck 'entenhausen *a*))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(assoc "Batman" '(("Superman" . "Metropolis")
                  ("Batman" . "Gotham City")))
(assoc "Batman" '(("Superman" . "Metropolis")
                  ("Batman" . "Gotham City"))
       :test 'string=)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(assoc "batman" '(("Superman" . "Metropolis")
                  ("Batman" . "Gotham City"))
       :test 'string=)
(assoc "batman" '(("Superman" . "Metropolis")
                  ("Batman" . "Gotham City"))
       :test 'string=
       :key 'string-downcase)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(assoc-if 'oddp '((2 . "two")
                  (4 . "four")
                  (3 . "three")
                  (5 . "five")))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setf *a* (cons (cons 'lois-lane 'metropolis) *a*))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(pairlis (list "Batman" "Superman" "Donald Duck")
         (list "Gotham City" "Metropolis" "Duckburg"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(rassoc "Metropolis" '(("Superman" . "Metropolis")
                       ("Batman" . "Gotham City"))
        :test 'string=)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setf (cdr (assoc 'batman *a*)) 'new-york-city)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setf (cdr (assoc 'spider-man *a*)) 'new-york-city)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setf (sys:cdr-assoc 'spider-man *a*) 'new-york-city)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defparameter *l* (list 'superman 'metropolis 'batman 'gotham-city))
(getf *l* 'batman)
(getf *l* 'donald-duck)
(getf *l* 'donald-duck 'nirvana)
(setf *l* (list* 'donald-duck 'duckburg *l*))
(getf *l* 'donald-duck)
(setf *l* (list* 'donald-duck 'entenhausen *l*))
(getf *l* 'donald-duck)
(remf *l* 'donald-duck)
*l*
(setf (getf *l* 'donald-duck) 'entenhausen)
*l*
(get-properties *l* '(batman superman))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defparameter *l*
  (loop for symbol in '(:a :b :c :d :e :f :g :h :i :j :k)
        ;; use ASCII code of symbol's character as value
        for code = (char-code (char (symbol-name symbol) 0))
        collect symbol
        collect code))
*l*
(let ((plist *l*) key value)
  (loop
   (multiple-value-setq (key value plist)
       (get-properties plist '(:f :j :a)))
   ;; leave loop if nothing was found
   (unless key (return))
   ;; skip key/value pair which was found
   (setf plist (cddr plist))
   ;; do something with the data
   (print (list key value))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; primes below 24
(defparameter *p* (list 2 3 5 7 11 13 17 19 23))
;; odd numbers below 24
(defparameter *o* (list 1 3 5 7 9 11 13 15 17 19 21 23))
(union *p* *o*)
(intersection *p* *o*)
(set-difference *p* *o*)
(set-difference *o* *p*)
(set-exclusive-or *p* *o*)
(subsetp *o* *p*)
(subsetp '(11 23) *p*)
(adjoin 2 *p*)
(adjoin 29 *p*)
(member 29 *p*)
(member 17 *p*)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(set-difference '("Groucho" "Chico" "Harpo") '("Groucho"))
(set-difference '("Groucho" "Chico" "Harpo") '("Groucho")
                :test 'string=)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun set-equal (a b)
  (null (set-exclusive-or a b)))
(set-equal '(1 2 2 3) '(3 3 1 1 2))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun hash-set-union (a b)
  (let ((result (make-hash-table)))
    (loop for key being the hash-keys of a
          do (setf (gethash key result) t))
    (loop for key being the hash-keys of b
          do (setf (gethash key result) t))
    result))

(defun hash-set-intersection (a b)
  (let ((result (make-hash-table)))
    (loop for key being the hash-keys of a
          when (gethash key b)
            do (setf (gethash key result) t))
    result))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; *A* is the set {0,1,3}
(defparameter *a* #b1011)
;; *B* is the set {0,3,4}
(defparameter *b* #b11001)
(setf *print-base* 2)
;; union
(logior *a* *b*)
;; intersection
(logand *a* *b*)
;; remove element 1 from set *A*
(setf (ldb (byte 1 1) *a*) 0)
*a*
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
