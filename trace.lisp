(in-package :xuriella)

(defparameter *xslt-trace* nil)
(defparameter *xslt-trace-context* nil)
(defparameter *xslt-trace-indent* 2)

(defvar *xslt-trace-level* 0)

(defun xslt-trace (fmt &rest args)
  (when *xslt-trace*
    (format *debug-io* "~&~v@T~?~%"
            (* *xslt-trace-level* *xslt-trace-indent*)
            fmt args)))

(defun xslt-trace-thunk (thunk fmt &rest other-args)
  (if *xslt-trace*
      #'(lambda (ctx)
          (format *debug-io* "~&~v@TENTER: ~?~%"
                  (* *xslt-trace-level* *xslt-trace-indent*)
                  fmt (substitute "..." :result other-args))
          (let ((result (let ((*xslt-trace-level* (1+ *xslt-trace-level*)))
                          (funcall thunk ctx))))
            (format *debug-io* "~&~v@TLEAVE: ~@[(CONTEXT: ~s)~%~]~?~%"
                    (* *xslt-trace-level* *xslt-trace-indent*)
                    (and *xslt-trace-context* (xpath:context-node ctx)) fmt
                    (substitute result :result other-args))
            result))
      thunk))