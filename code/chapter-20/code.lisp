;;; Copyright (c) 2015, Edmund Weitz.  All rights reserved.

;;; This is example code for the book "Common Lisp Recipes" and meant
;;; to be used with something like (from SLIME) C-M-x or C-c C-c.
;;; See the book for more information.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (error "This code is not meant to be used with LOAD or COMPILE-FILE."))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; see file "index.html" in this directory
(hunchentoot:start
  (make-instance 'hunchentoot:easy-acceptor
                 :document-root "/path/to/gui/"
                 :port 4242))

(hunchentoot:define-easy-handler (get-symbols :uri "/get-symbols")
    (term)
  (setf (hunchentoot:content-type*) "application/json")
  (with-output-to-string (*standard-output*)
    (yason:encode
     (sort
      (mapcar 'string-downcase (apropos-list term :cl))
      'string<))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defparameter *size* 400)

(defun gui ()
  (ltk:with-ltk ()
    (let* ((vals (list 2 3 4))
           (canvas (ltk:make-canvas nil :width *size* :height *size*))
           (spinbox
            (make-instance 'ltk:spinbox
                           :width 3
                           :command (lambda (val)
                                      (sierpinski canvas
                                                  (parse-integer val)))
                           :master nil
                           :values vals
                           :text (first vals))))
      (ltk:wm-title ltk:*tk* "Sierpinski")
      (ltk:configure canvas :background :white)
      (ltk:pack canvas)
      (ltk:pack spinbox)
      (sierpinski canvas (first vals)))))

(defun sierpinski (canvas level)
  (ltk:clear canvas)
  (labels ((square (x y size)
             (let ((rectangle
                    (ltk:create-rectangle canvas x y
                                          (+ x size) (+ y size))))
               (ltk:itemconfigure canvas rectangle :fill :red)
               (ltk:itemconfigure canvas rectangle :outline :red)))
           (recurse (x y size level)
             (let ((step (* 1/3 size)))
               (square (+ x step) (+ y step) step)
               (when (plusp level)
                 (dolist (next-x (list x (+ x step) (+ x step step)))
                   (dolist (next-y (list y (+ y step) (+ y step step)))
                     (recurse next-x next-y step (1- level))))))))
    (recurse 0 0 *size* level)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'abcl-contrib)
(require 'jss)
(use-package :jss)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun say-hello ()
  (let ((frame (new 'JFrame "Hello ABCL"))
        (label (new 'JLabel
                    "The crux of the biscuit is the apostrophe.")))
    (#"add" (#"getContentPane" frame) label)
    (#"pack" frame)
    (#"setVisible" frame t)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(#"invokeLater" 'SwingUtilities
                (jinterface-implementation "java.lang.Runnable"
                                           "run" #'say-hello))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; see Java files in this directory
(add-to-classpath "/tmp/")
(require 'abcl-contrib)
(require 'jss)
(use-package :jss)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defparameter *size* 400)

(defun set-points (java-array)
  (loop for x from -2d0 to 1d0 by (/ 3d0 *size*)
        for i from 0 do
        (loop for y from 1.5d0 downto -1.5d0 by (/ 3d0 *size*)
              for j from 0
              for c = (complex x y)
              when (loop repeat 100
                         for z = c then (+ (* z z) c)
                         always (< (abs z) 2d0)) do
                (jarray-set java-array +true+ i j))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(let ((mandelbrot
       (new "de.weitz.Mandelbrot"
            (jinterface-implementation "de.weitz.PointSetter"
                                       "fill" #'set-points)
            *size*)))
  (#"display" mandelbrot))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass node ()
  ((value :initarg :value
          :reader value)
   (children :initform nil
             :accessor children)))

(defmethod print-object ((node node) stream)
  (with-slots (value) node
    (format stream "~A/~A" (numerator value) (denominator value))))

(defmethod add-children ((node node))
  (let* ((numerator (numerator (value node)))
         (denominator (denominator (value node)))
         (sum (+ numerator denominator)))
    (setf (children node)
          (list (make-instance 'node :value (/ numerator sum))
                (make-instance 'node :value (/ sum denominator))))))

(defun one ()
  (make-instance 'node :value 1))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(capi:define-interface calkin-wilf-tree ()
  () 
  (:panes
   (tree
    capi:tree-view
    :reader tree
    :roots (list (one))
    :children-function #'children
    :action-callback (lambda (node interface)
                       (unless (children node)
                         (add-children node)
                         (capi:tree-view-update-item (tree interface)
                                                     node nil)))
    :action-callback-expand-p t)
   (reset-button
    capi:push-button
    :text "Reset"
    :callback-type :interface
    :callback (lambda (interface)
                (setf (capi:tree-view-roots
                       (tree interface))
                      (list (one))))))
  (:layouts
   (default-layout
    capi:column-layout
    '(tree reset-button)
    :adjust :center))
  (:default-initargs
   :best-width 400
   :best-height 400
   :title "Calkin-Wilf Tree"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(capi:display (make-instance 'calkin-wilf-tree))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
