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
(defparameter *declared-variables* '())
(defparameter *variable-values* '())

(defclass xslt-environment () ())

(defun make-xslt-environment ()
  (make-instance 'xslt-environment))

(defun decode-qname (qname env attributep)
  (multiple-value-bind (prefix local-name)
      (cxml::split-qname qname)
    (values local-name
	    (if (or prefix (not attributep))
		(xpath:environment-find-namespace env prefix)
		""))))

(defmethod xpath:environment-find-namespace ((env xslt-environment) prefix)
  (cdr (assoc prefix *namespaces* :test 'equal)))

(defmethod xpath:environment-validate-variable
    ((env xslt-environment) lname uri)
  (find (cons lname uri) *declared-variables* :test 'equal))


(defstruct (xslt-context (:include xpath::context)))

(defmethod xpath:context-variable-value ((ctx xslt-context) local-name uri)
  (cdr (assoc (cons local-name uri) *variable-values* :test 'equal)))


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

(defvar *xsl* "http://www.w3.org/1999/XSL/Transform")
(defvar *xml* "http://www.w3.org/XML/1998/namespace")

(defun of-name (local-name)
  (stp:of-name local-name *xsl*))

(defun namep (node local-name)
  (and (equal (stp:namespace-uri node) *xsl*)
       (equal (stp:local-name node) local-name)))


;;;; Whitespace

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *whitespace*
    (format nil "~C~C~C~C"
	    (code-char 9)
	    (code-char 32)
	    (code-char 13)
	    (code-char 10))))

(defun normalize-whitespace (str)
  (cl-ppcre:regex-replace-all #.(format nil "[~A]+" *whitespace*)
			      (string-trim *whitespace* str)
			      " "))

(defun whitespacep (str)
  (cl-ppcre:all-matches #.(format nil "^[~A]+$" *whitespace*) str))

;; For stylesheets, not source documents.  Also strips comments and PIs.
(defun strip-stylesheet (parent &optional preserve)
  (let ((i 0))
    (loop while (< i (length (cxml-stp-impl::%children parent))) do
	 (let ((child (stp:nth-child i parent)))
	   (etypecase child
	     (stp:text
	      (if (and (whitespacep (stp:data child))
		       (not preserve))
		  (stp:delete-nth-child i parent)
		  (incf i)))
	     ((or stp:comment stp:processing-instruction)
	      (stp:delete-nth-child i parent))
	     (stp:element
		 (stp:with-attributes ((space "space" *xml*))
		     child
		   (let ((new-preserve
			  (cond
			    ((namep child "text") t)
			    ((not space) preserve)
			    ((equal space "preserve") t)
			    (t nil))))
		     (strip-stylesheet child new-preserve)))
	       (incf i)))))))


;;;; PARSE-STYLESHEET

(defstruct stylesheet
  (modes (make-hash-table :test 'equal))
  (html-output-p nil))

(defstruct mode
  (named-templates (make-hash-table :test 'equal))
  (other-templates nil))

(defun find-mode (mode stylesheet)
  (gethash mode (stylesheet-modes stylesheet)))

(defun ensure-mode (mode stylesheet)
  (or (find-mode mode stylesheet)
      (setf (gethash mode (stylesheet-modes stylesheet))
	    (make-mode))))

(defun parse-stylesheet (d)
  ;; FIXME: I was originally planning on rewriting this using klacks
  ;; eventually, but now let's just build an STP document
  (let* ((d (cxml:parse d (cxml-stp:make-builder)))
	 (<transform> (stp:document-element d))
	 (stylesheet (make-stylesheet))
	 (env (make-xslt-environment)))
    (strip-stylesheet <transform>)
    ;; FIXME: handle embedded stylesheets
    (unless (and (equal (stp:namespace-uri <transform>) *xsl*)
		 (or (equal (stp:local-name <transform>) "transform")
		     (equal (stp:local-name <transform>) "stylesheet")))
      (error "not a stylesheet"))
    (ensure-mode "" stylesheet)
    (dolist (<template> (stp:filter-children (of-name "template") <transform>))
      (dolist (template (compile-template <template> env))
	(let ((mode (ensure-mode (template-mode template) stylesheet))
	      (name-test (template-qname-test template)))
	  (if name-test
	      (multiple-value-bind (local-name uri)
		  (decode-qname name-test env nil)
		(push template
		      (gethash (cons local-name uri)
			       (mode-named-templates mode))))
	      (push template (mode-other-templates mode))))))
    stylesheet))


;;;; APPLY-STYLESHEET

(defvar *mode*)

(defun apply-stylesheet (stylesheet source-document &optional output-spec)
  (when (typep source-document
	       '(or runes:xstream runes:rod array stream pathname))
    (setf source-document (cxml:parse source-document (stp:make-builder))))
  (invoke-with-output-sink
   (lambda ()
     (let ((*mode* (find-mode "" stylesheet)))
       (apply-templates (xpath::make-context source-document))))
   stylesheet
   output-spec))

(defun apply-templates/list (list)
  (let* ((n (length list))
	 (s/d (lambda () n)))
    (loop
       for i from 1
       for child in list
       do
	 (apply-templates (xpath::make-context child s/d i)))))

(defun apply-templates (ctx)
  (let ((template (find-template ctx)))
    (if template
	(funcall (template-body template) ctx)
	(let ((node (xpath::context-node ctx)))
	  (cond
	    ((or (xpath-protocol:node-type-p node :processing-instruction)
		 (xpath-protocol:node-type-p node :comment)))
	    ((xpath-protocol:node-type-p node :text)
	     (cxml:text (xpath-protocol:string-value node)))
	    (t
	     (apply-templates/list
	      (xpath::force
	       (xpath-protocol:child-pipe node)))))))))

(defun find-template (ctx)
  (let* ((node
	  (xpath::context-node ctx))
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
  (find (xpath::context-node ctx)
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
     (cxml:with-xml-output (make-output-sink stylesheet output)
       (funcall fn)))
    ((or hax:abstract-handler sax:abstract-handler)
     (cxml:with-xml-output output
       (funcall fn)))))

(defun make-output-sink (stylesheet stream)
  (if (stylesheet-html-output-p stylesheet)
      (if stream
	  (let ((et (stream-element-type stream)))
	    (cond
	      ((or (null et) (subtypep et '(unsigned-byte 8)))
	       (chtml:make-character-stream-sink stream))
	      ((subtypep et 'character)
	       (chtml:make-octet-stream-sink stream))))
	  (chtml:make-string-sink))
      (if stream
	  (let ((et (stream-element-type stream)))
	    (cond
	      ((or (null et) (subtypep et '(unsigned-byte 8)))
	       (cxml:make-character-stream-sink stream))
	      ((subtypep et 'character)
	       (cxml:make-octet-stream-sink stream))))
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
		       (xpath:compile-xpath expression env))
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
