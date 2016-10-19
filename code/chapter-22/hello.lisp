#-:abcl
(defun hello ()
  (format t "Hello World!~%The time is ~A.~%" (get-universal-time)))

#+:abcl
(defun hello (name)
  (format nil "Hello ~A!~%The time is ~A.~%"
          name (get-universal-time)))
