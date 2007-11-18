;;; -*- show-trailing-whitespace: t; indent-tabs: nil -*-

;;; Copyright (c) 2007 David Lichteblau. All rights reserved.

;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:
;;;
;;;   * Redistributions of source code must retain the above copyright
;;;     notice, this list of conditions and the following disclaimer.
;;;
;;;   * Redistributions in binary form must reproduce the above
;;;     copyright notice, this list of conditions and the following
;;;     disclaimer in the documentation and/or other materials
;;;     provided with the distribution.
;;;
;;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR 'AS IS' AND ANY EXPRESSED
;;; OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
;;; DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
;;; GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
;;; WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

(in-package :xuriella)


;;;; Instructions

(defmacro define-instruction (name (args-var env-var) &body body)
  `(setf (get ',name 'xslt-instruction)
	 (lambda (,args-var ,env-var)
	   (declare (ignorable ,env-var))
	   ,@body)))

(define-instruction if (args env)
  (destructuring-bind (test then &optional else) args
    (let ((test-thunk (xpath:compile-xpath (xpath:parse-xpath test) env))
	  (then-thunk (compile-instruction then env))
	  (else-thunk (when else (compile-instruction else env))))
      (lambda (ctx)
	(cond
	  ((xpath:boolean-value (funcall test-thunk ctx))
	   (funcall then-thunk ctx))
	  (else-thunk
	   (funcall else-thunk ctx)))))))

(define-instruction when (args env)
  (destructuring-bind (test &rest body) args
    (compile-instruction `(if ,test (progn ,@body)) env)))

(define-instruction unless (args env)
  (destructuring-bind (test &rest body) args
    (compile-instruction `(if (:not ,test) (progn ,@body)) env)))

(define-instruction cond (args env)
  (if args
      (destructuring-bind ((test &body body) &rest clauses) args
	(compile-instruction `(if ,test
				  (progn ,@body)
				  (cond ,@clauses))
			     env))
      (constantly nil)))

(define-instruction progn (args env)
  (if args
      (let ((first-thunk (compile-instruction (first args) env))
	    (rest-thunk (compile-instruction `(progn ,@(rest args)) env)))
	(lambda (ctx)
	  (funcall first-thunk ctx)
	  (funcall rest-thunk ctx)))
      (constantly nil)))

(define-instruction element (args env)
  (destructuring-bind ((name &key namespace use-attribute-sets)
		       &body body)
      args
    (declare (ignore namespace use-attribute-sets))	;fixme
    (let ((name-thunk (compile-attribute-value-template name env))
	  (body-thunk (compile-instruction `(progn ,@body) env)))
      (lambda (ctx)
	(cxml:with-element (funcall name-thunk ctx)
	  (funcall body-thunk ctx))))))

(define-instruction attribute (args env)
  (destructuring-bind ((name &key namespace) &body body) args
    (declare (ignore namespace))	;fixme
    (let ((name-thunk (compile-attribute-value-template name env))
	  (value-thunk (compile-instruction `(progn ,@body) env)))
      (lambda (ctx)
	(cxml:attribute
	 (funcall name-thunk ctx)
	 (with-text-output-sink (s)
	   (cxml:with-xml-output s
	     (funcall value-thunk ctx))))))))

(define-instruction text (args env)
  (destructuring-bind (str) args
    (lambda (ctx)
      (declare (ignore ctx))
      (cxml:text str))))

(define-instruction processing-instruction (args env)
  (destructuring-bind (name &rest body) args
    (let ((name-thunk (compile-attribute-value-template name env))
	  (value-thunk (compile-instruction `(progn ,@body) env)))
      (lambda (ctx)
	(cxml:processing-instruction
	 (funcall name-thunk ctx)
	 (with-text-output-sink (s)
	   (cxml:with-xml-output s
	     (funcall value-thunk ctx))))))))

(define-instruction comment (args env)
  (destructuring-bind (str) args
    (lambda (ctx)
      (declare (ignore ctx))
      (cxml:comment str))))

(define-instruction value-of (args env)
  (destructuring-bind (xpath) args
    (let ((thunk (xpath:compile-xpath (xpath:parse-xpath xpath) env)))
      (lambda (ctx)
	(cxml:text (xpath:string-value (funcall thunk ctx)))))))

(define-instruction unescaped-value-of (args env)
  (destructuring-bind (xpath) args
    (let ((thunk (xpath:compile-xpath (xpath:parse-xpath xpath) env)))
      (lambda (ctx)
	(cxml:unescaped (xpath:string-value (funcall thunk ctx)))))))

(define-instruction for-each (args env)
  (destructuring-bind (select &optional decls &rest body) args
    (when (and (consp decls)
	       (not (eq (car decls) 'declare)))
      (push decls body)
      (setf decls nil))
    (let ((select-thunk (xpath:compile-xpath (xpath:parse-xpath select) env))
	  (body-thunk (compile-instruction `(progn ,@body) env))
	  (sorter
	   ;; fixme: parse decls here
	   #'identity))
      (lambda (ctx)
	(let ((node-set (funcall sorter (funcall select-thunk ctx))))
	  (dolist (node node-set)
	    (funcall body-thunk
		     (make-xslt-context :node node :node-set node-set))))))))

(define-instruction with-namespaces (args env)
  (destructuring-bind ((&rest forms) &rest body) args
    (let ((*namespaces* *namespaces*))
      (dolist (form forms)
	(destructuring-bind (prefix uri) form
	  (push (cons prefix uri) *namespaces*)))
      (compile-instruction `(progn ,@body) env))))

(define-instruction let (args env)
  (destructuring-bind ((&rest forms) &rest body) args
    (let ((declared-variables *declared-variables*)
	  (vars '()))
      (dolist (form forms)
	(destructuring-bind (name value) form
	  (multiple-value-bind (local-name uri)
	      (decode-qname name env nil)
	    (let ((pair (cons local-name uri))
		  (var-thunk
		   (if (and (listp value) (eq (car value) 'progn))
		       (compile-instruction value env)
		       (xpath:compile-xpath (xpath:parse-xpath value)
					    env))))
	      (when (find pair *declared-variables* :test 'equal)
		(error "duplicate definition of ~A" name))
	      (push pair declared-variables)
	      (push (cons pair var-thunk) vars)))))
      (let* ((*declared-variables* declared-variables)
	     (thunk (compile-instruction `(progn ,@body) env)))
	(lambda (ctx)
	  (let ((*variable-values* *variable-values*))
	    (loop
	       for (pair . var-thunk) in vars
	       do (push (cons pair (funcall var-thunk ctx))
			*variable-values*))
	    (funcall thunk ctx)))))))

(define-instruction let* (args env)
  (destructuring-bind ((&rest forms) &rest body) args
    (if forms
	(compile-instruction `(let (,(car forms))
				(let* (,@(cdr forms))
				  ,@body))
			     env)
	(compile-instruction `(progn ,@body) env))))

(define-instruction message (args env)
  (compile-message #'warn args env))

(define-instruction terminate (args env)
  (compile-message #'error args env))

(defun compile-message (fn args env)
  (let ((thunk (compile-instruction `(progn ,@args) env)))
    (lambda (ctx)
      (funcall fn
	       (cxml:with-xml-output (cxml:make-string-sink)
		 (funcall thunk ctx))))))

(defun compile-instruction (form env)
  (funcall (get (car form) 'xslt-instruction)
	   (cdr form)
	   env))

(defun compile-attribute-value-template (str env)
  (declare (ignore env))
  ;; fixme
  (constantly str))


;;;;

(defun test-instruction (form document)
  (let ((thunk (compile-instruction form (make-xslt-environment)))
	(root (cxml:parse document (cxml-dom:make-dom-builder))))
    (cxml:with-xml-output (cxml:make-string-sink)
      (funcall thunk (make-xslt-context :node root)))))
