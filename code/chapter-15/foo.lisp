(defparameter *foo* *load-pathname*)

;; remove comment, then delete FASL:
;; (defparameter *bar* *compile-file-pathname*)

;; alternative, see book:
;; (eval-when (:compile-toplevel)
;;   (defparameter *bar* *compile-file-pathname*))
