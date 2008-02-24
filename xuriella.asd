;;; -*- mode: lisp -*-
(defpackage :xuriella-system
  (:use :asdf :cl))
(in-package :xuriella-system)

(defclass closure-source-file (cl-source-file) ())

#+sbcl
(defmethod perform :around ((o compile-op) (s closure-source-file))
  ;; shut up already.  Correctness first.
  (handler-bind ((sb-ext:compiler-note #'muffle-warning))
    (let ((*compile-print* nil))
      (call-next-method))))

(defsystem :xuriella
    :default-component-class closure-source-file
    :serial t
    :components
    ((:file "package")
     (:file "trace")
     (:file "unparse")
     (:file "xslt")
     (:file "instructions")
     (:file "parser")
     (:file "space")
     (:file "html")
     (:file "format-number")
     (:file "number")
     (:file "test"))
    :depends-on (:cxml :cxml-stp :closure-html :xpath :split-sequence))
