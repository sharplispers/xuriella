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

#+sbcl
(declaim (optimize (debug 2)))


;;;; XSLT-ENVIRONMENT and XSLT-CONTEXT

(defparameter *namespaces*
  '((nil . "")
    ("xmlns" . #"http://www.w3.org/2000/xmlns/")
    ("xml" . #"http://www.w3.org/XML/1998/namespace")))

(defparameter *variables* '())

(defclass xslt-environment () ())

(defun decode-qname (qname env attributep)
  (multiple-value-bind (prefix local-name)
      (cxml::split-qname qname)
    (values local-name
	    (if (or prefix (not attributep))
		(xpath:environment-find-namespace env prefix)
		""))))

(defmethod xpath:environment-find-namespace ((env xslt-environment) prefix)
  (cdr (assoc prefix *namespaces* :test 'equal)))

(defclass lexical-xslt-environment (xslt-environment) ())

(defmethod xpath:environment-find-variable
    ((env lexical-xslt-environment) lname uri)
  (let ((gensym (cdr (assoc (cons lname uri) *variables* :test 'equal))))
    (and gensym
	 (lambda (ctx)
	   (declare (ignore ctx))
	   (symbol-value gensym)))))

(defclass global-variable-environment (xslt-environment)
  ((global-variables :initarg :global-variables :accessor global-variables)))

(defmethod xpath:environment-find-variable
    ((env global-variable-environment) lname uri)
  (gethash (cons lname uri) (global-variables env)))


;;;; TEXT-OUTPUT-SINK
;;;;
;;;; A sink that serializes only text and will error out on any other
;;;; SAX event.

(defmacro with-text-output-sink ((var) &body body)
  `(invoke-with-text-output-sink (lambda (,var) ,@body)))

(defclass text-output-sink (sax:content-handler)
  ((target :initarg :target :accessor text-output-sink-target)))

(defmethod sax:characters ((sink text-output-sink) data)
  (write-string data (text-output-sink-target sink)))

(defun invoke-with-text-output-sink (fn)
  (with-output-to-string (s)
    (funcall fn (make-instance 'text-output-sink :target s))))


;;;; Names

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *xsl* "http://www.w3.org/1999/XSL/Transform")
  (defvar *xml* "http://www.w3.org/XML/1998/namespace"))

(defun of-name (local-name)
  (stp:of-name local-name *xsl*))

(defun namep (node local-name)
  (and (typep node '(or stp:element stp:attribute))
       (equal (stp:namespace-uri node) *xsl*)
       (equal (stp:local-name node) local-name)))


;;;; PARSE-STYLESHEET

(defstruct stylesheet
  (modes (make-hash-table :test 'equal))
  (html-output-p nil)
  (global-variables ())
  (output-specification (make-output-specification))
  (strip-tests nil))

(defstruct mode
  (named-templates (make-hash-table :test 'equal))
  (other-templates nil))

(defun find-mode (mode stylesheet)
  (gethash mode (stylesheet-modes stylesheet)))

(defun ensure-mode (mode stylesheet)
  (or (find-mode mode stylesheet)
      (setf (gethash mode (stylesheet-modes stylesheet))
	    (make-mode))))

(defun acons-namespaces (element &optional (bindings *namespaces*))
  (map-namespace-declarations (lambda (prefix uri)
				(push (cons prefix uri) bindings))
			      element)
  bindings)

(defun parse-stylesheet (d)
  (let* ((d (cxml:parse d (cxml-stp:make-builder)))
	 (<transform> (stp:document-element d))
	 (*namespaces* (acons-namespaces <transform>))
	 (*variables* nil)
	 (stylesheet (make-stylesheet))
	 (env (make-instance 'lexical-xslt-environment)))
    (strip-stylesheet <transform>)
    ;; FIXME: handle embedded stylesheets
    (unless (and (equal (stp:namespace-uri <transform>) *xsl*)
		 (or (equal (stp:local-name <transform>) "transform")
		     (equal (stp:local-name <transform>) "stylesheet")))
      (error "not a stylesheet"))
    (ensure-mode "" stylesheet)
    (parse-global-variables! stylesheet <transform>)
    (parse-templates! stylesheet <transform> env)
    (parse-output! stylesheet <transform>)
    (parse-strip/preserve-space! stylesheet <transform> env)
    stylesheet))

(xpath:with-namespaces ((nil #.*xsl*))
  (defun parse-strip/preserve-space! (stylesheet <transform> env)
    (xpath:do-node-set
	(elt (xpath:evaluate "strip-space|preserve-space" <transform>))
      (let ((*namespaces* (acons-namespaces elt))
	    (mode
	     (if (equal (stp:local-name elt) "strip-space")
		 :strip
		 :preserve)))
	(dolist (name-test (words (stp:attribute-value elt "elements")))
	  (let* ((pos (search ":*" name-test))
		 (test-function
		  (cond
		    ((eql pos (- (length name-test) 2))
		     (let* ((prefix (subseq name-test 0 pos))
			    (name-test-uri
			     (xpath:environment-find-namespace env prefix)))
		       (unless (xpath::nc-name-p prefix)
			 (error "not an NCName: ~A" prefix))
		       (lambda (local-name uri)
			 (declare (ignore local-name))
			 (if (equal uri name-test-uri)
			     mode
			     nil))))
		    ((equal name-test "*")
		     (lambda (local-name uri)
		       (declare (ignore local-name uri))
		       mode))
		    (t
		     (multiple-value-bind (name-test-local-name name-test-uri)
			 (decode-qname name-test env nil)
		       (lambda (local-name uri)
			 (if (and (equal local-name name-test-local-name)
				  (equal uri name-test-uri))
			     mode
			     nil)))))))
	    (push test-function (stylesheet-strip-tests stylesheet))))))))

(defstruct (output-specification
	     (:conc-name "OUTPUT-"))
  indent
  omit-xml-declaration)

(defun parse-output! (stylesheet <transform>)
  (let ((outputs (stp:filter-children (of-name "output") <transform>)))
    (when outputs
      (when (cdr outputs)
	;; FIXME:
	;;   - concatenate cdata-section-elements
	;;   - the others must not conflict
	(error "oops, merging of output elements not supported yet"))
      (let ((<output> (car outputs))
	    (spec (stylesheet-output-specification stylesheet)))
	(stp:with-attributes (;; version
			      indent
;;; 			      encoding
;;; 			      media-type
;;; 			      doctype-system
;;; 			      doctype-public
 			      omit-xml-declaration
;;; 			      standalone
;;; 			      cdata-section-elements
			      )
	    <output>
	  (setf (output-indent spec) indent)
	  (setf (output-omit-xml-declaration spec) omit-xml-declaration))))))

(defun compile-global-variable (<variable> env) ;; also for <param>
  (stp:with-attributes (name select) <variable>
    (when (and select (stp:list-children <variable>))
      (error "variable with select and body"))
    (cond
      (select
	(xpath:compile-xpath select env))
      ((stp:list-children <variable>)
       (let* ((inner-sexpr `(progn ,@(parse-body <variable>)))
	      (inner-thunk (compile-instruction inner-sexpr env)))
	 (lambda (ctx)
	   (apply-to-result-tree-fragment ctx inner-thunk))))
      (t
       (lambda (ctx)
	 (declare (ignore ctx))
	 "")))))

(defstruct (variable-information
	     (:constructor make-variable)
	     (:conc-name "VARIABLE-"))
  gensym
  thunk
  local-name
  uri
  param-p
  thunk-setter)

(defun parse-global-variable! (<variable> global-env) ;; also for <param>
  (let ((*namespaces* (acons-namespaces <variable>))
	(qname (stp:attribute-value <variable> "name")))
    (multiple-value-bind (local-name uri)
	(decode-qname qname global-env nil)
      (let ((key (cons local-name uri))
	    (gensym (gensym local-name)))
	;; For the normal compilation environment of templates, install it
	;; into *VARIABLES* using its gensym:
	(push (cons key gensym) *variables*)
	;; For the evaluation of a global variable itself, build a thunk
	;; that lazily resolves other variables:
	(let* ((value-thunk :unknown)
	       (global-variable-thunk
		(lambda (ctx)
		  (when (eq (symbol-value gensym) 'seen)
		    (error "recursive variable definition"))
		  (or (symbol-value gensym)
		      (progn
			(setf (symbol-value gensym) 'seen)
			(setf (symbol-value gensym)
			      (funcall value-thunk ctx))))))
	       (thunk-setter
		(lambda ()
		  (setf value-thunk
			(compile-global-variable <variable> global-env)))))
	  (setf (gethash key (global-variables global-env))
		global-variable-thunk)
	  (make-variable :gensym gensym
			 :local-name local-name
			 :uri uri
			 :thunk global-variable-thunk
			 :param-p (namep <variable> "param")
			 :thunk-setter thunk-setter))))))

(xpath:with-namespaces ((nil #.*xsl*))
  (defun parse-global-variables! (stylesheet <transform>)
    (let* ((table (make-hash-table :test 'equal))
	   (global-env (make-instance 'global-variable-environment
				      :global-variables table))
	   (specs
	    (mapcar (lambda (<variable>)
		      (parse-global-variable! <variable> global-env))
		    (xpath:all-nodes
		     (xpath:evaluate "variable|param" <transform>)))))
      ;; now that the global environment knows about all variables, run the
      ;; thunk setters to perform their compilation
      (mapc (lambda (spec) (funcall (variable-thunk-setter spec))) specs)
      (setf (stylesheet-global-variables stylesheet) specs))))

(defun parse-templates! (stylesheet <transform> env)
  (dolist (<template> (stp:filter-children (of-name "template") <transform>))
    (let ((*namespaces* (acons-namespaces <template>)))
      (dolist (template (compile-template <template> env))
	(let ((mode (ensure-mode (template-mode template) stylesheet))
	      (name-test (template-qname-test template)))
	  (if name-test
	      (multiple-value-bind (local-name uri)
		  (decode-qname name-test env nil)
		(push template
		      (gethash (cons local-name uri)
			       (mode-named-templates mode))))
	      (push template (mode-other-templates mode))))))))


;;;; APPLY-STYLESHEET

(defvar *stylesheet*)
(defvar *mode*)

(deftype xml-designator () '(or runes:xstream runes:rod array stream pathname))

(defstruct (parameter
	     (:constructor make-parameter (value local-name &optional uri)))
  (uri "")
  local-name
  value)

(defun find-parameter-value (local-name uri parameters)
  (dolist (p parameters)
    (when (and (equal (parameter-local-name p) local-name)
	       (equal (parameter-uri p) uri))
      (return (parameter-value p)))))

(defun apply-stylesheet
    (stylesheet source-document &key output parameters)
  (when (typep stylesheet 'xml-designator)
    (setf stylesheet (parse-stylesheet stylesheet)))
  (when (typep source-document 'xml-designator)
    (setf source-document (cxml:parse source-document (stp:make-builder))))
  (invoke-with-output-sink
   (lambda ()
     (let ((*stylesheet* stylesheet)
	   (*mode* (find-mode "" stylesheet))
	   (globals (stylesheet-global-variables stylesheet))
	   (ctx (xpath:make-context
		 (make-whitespace-stripper
		  source-document
		  (stylesheet-strip-tests stylesheet)))))
       (progv
	   (mapcar #'variable-gensym globals)
	   (make-list (length globals) :initial-element nil)
	 (mapc (lambda (spec)
		 (when (variable-param-p spec)
		   (let ((value
			  (find-parameter-value (variable-local-name spec)
						(variable-uri spec)
						parameters)))
		     (when value
		       (setf (symbol-value (variable-gensym spec)) value)))))
	       globals)
	 (mapc (lambda (spec)
		 (funcall (variable-thunk spec) ctx))
	       globals)
	 (apply-templates ctx)
;;; 	 (when (equal (output-indent
;;; 		       (stylesheet-output-specification stylesheet))
;;; 		      "yes")
;;; 	   Hack: We don't have indentation support yet, but at least make
;;; 	   sure to add a newline at the end so that our output matches
;;; 	   what xsltproc does.
;;; 	   (cxml::sink-fresh-line cxml::*sink*))
	 ;; Turns out xsltproc does it unconditionally:
	 (cxml::%write-rune #\newline cxml::*sink*))))
   stylesheet
   output))

(defun apply-templates/list (list)
  (let* ((n (length list))
	 (s/d (lambda () n)))
    (loop
       for i from 1
       for child in list
       do
	 (apply-templates (xpath:make-context child s/d i)))))

(defun apply-templates (ctx)
  (let ((template (find-template ctx)))
    (if template
	(funcall (template-body template) ctx)
	(let ((node (xpath:context-node ctx)))
	  (cond
	    ((or (xpath-protocol:node-type-p node :processing-instruction)
		 (xpath-protocol:node-type-p node :comment)))
	    ((or (xpath-protocol:node-type-p node :text)
		 (xpath-protocol:node-type-p node :attribute))
	     (cxml:text (xpath-protocol:string-value node)))
	    (t
	     (apply-templates/list
	      (xpath::force
	       (xpath-protocol:child-pipe node)))))))))

(defun find-template (ctx)
  (let* ((node
	  (xpath:context-node ctx))
	 (key
	  (when (xpath-protocol:node-type-p node :element)
	    (cons (xpath-protocol:local-name node)
		  (xpath-protocol:namespace-uri node))))
	 (templates
	  (append (and key (gethash key (mode-named-templates *mode*)))
		  (mode-other-templates *mode*)))
	 (matching-candidates
	  (remove-if-not (lambda (template)
			   (template-matches-p template ctx))
			 templates)))
    (maximize #'template< matching-candidates)))

(defun template< (a b)
  (let ((i (template-import-precedence a))
	(j (template-import-precedence b))
	(p (template-priority a))
	(q (template-priority b)))
    (cond
      ((< i j) t)
      ((> i j) nil)
      (t
       (cond
	 ((< p q) t)
	 ((> p q) nil)
	 (t
	  (error "conflicting templates: ~A, ~A" a b)))))))

(defun maximize (< things)
  (when things
    (let ((max (car things)))
      (dolist (other (cdr things))
	(when (funcall < max other)
	  (setf max other)))
      max)))

(defun template-matches-p (template ctx)
  (find (xpath:context-node ctx)
	(xpath:all-nodes (funcall (template-match-thunk template) ctx))))

(defun invoke-with-output-sink (fn stylesheet output)
  (etypecase output
    (pathname
     (with-open-file (s output
			:direction :output
			:element-type '(unsigned-byte 8)
			:if-exists :rename-and-delete)
       (invoke-with-output-sink fn stylesheet s)))
    ((or stream null)
     (invoke-with-output-sink fn
			      stylesheet
			      (make-output-sink stylesheet output)))
    ((or hax:abstract-handler sax:abstract-handler)
     (when (equal (output-omit-xml-declaration
		   (stylesheet-output-specification stylesheet))
		  "yes")
       (setf (cxml:omit-xml-declaration-p output) t))
     (cxml:with-xml-output output
       (funcall fn)))))

(defun make-output-sink (stylesheet stream)
  (if (stylesheet-html-output-p stylesheet)
      (if stream
	  (let ((et (stream-element-type stream)))
	    (cond
	      ((or (null et) (subtypep et '(unsigned-byte 8)))
	       (chtml:make-octet-stream-sink stream))
	      ((subtypep et 'character)
	       (chtml:make-character-stream-sink stream))))
	  (chtml:make-string-sink))
      (if stream
	  (let ((et (stream-element-type stream)))
	    (cond
	      ((or (null et) (subtypep et '(unsigned-byte 8)))
	       (cxml:make-octet-stream-sink stream))
	      ((subtypep et 'character)
	       (cxml:make-character-stream-sink stream))))
	  (cxml:make-string-sink))))

(defstruct template
  match-expression
  match-thunk
  name
  priority
  mode
  body)

(defun template-import-precedence (template)
  template
  ;; fixme
  0)

(defun template-qname-test (template)
  (let* ((form (template-match-expression template))
	 (first-step (second form)))
    (when (and (null (cddr form))
	       (eq :child (car first-step)))
      (second first-step))))

(defun expression-priority (form)
  (let ((first-step (second form)))
    (if (and (null (cddr form))
	     (eq :child (car first-step))
	     (null (cddr first-step)))
	(let ((name (second first-step)))
	  (cond
	    ((or (stringp name)
		 (eq (car name) :qname)
		 (eq (car name) :processing-instruction))
	     0.0)
	    ((eq (car name) :namespace)
	     -0.25)
	    (t
	     -0.5)))
	0.5)))

(defun parse-pattern (str)
  ;; zzz check here for anything not allowed as an XSLT pattern
  ;; zzz can we hack id() and key() here?
  (let ((form (xpath:parse-xpath str)))
    (unless (consp form)
      (error "not a valid pattern: ~A" str))
    (mapcar (lambda (case)
	      (unless (eq (car case) :path) ;zzz: filter statt path
		(error "not a valid pattern: ~A" str))
	      `(:path (:ancestor-or-self :node) ,@(cdr case)))
	    (if (eq (car form) :union)
		(cdr form)
		(list form)))))

(defun compile-template (<template> env)
  (stp:with-attributes (match name priority mode) <template>
    (unless (or name match)
      (error "missing match in template"))
    (let ((body (parse-body <template>)))
      (mapcar (lambda (expression)
		(let ((body-thunk
		       (compile-instruction `(progn ,@body) env))
		      (match-thunk
		       (xpath:compile-xpath `(xpath:xpath ,expression) env))
		      (p (if priority
			     (parse-number:parse-number priority)
			     (expression-priority expression))))
		  (make-template :match-expression expression
				 :match-thunk match-thunk
				 :name name
				 :priority p
				 :mode (or mode "")
				 :body body-thunk)))
	      (parse-pattern match)))))

#+(or)
(xuriella::parse-stylesheet #p"/home/david/src/lisp/xuriella/test.xsl")
