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


;;;; XSLT-ERROR

(define-condition xslt-error (simple-error)
  ()
  (:documentation "The class of all XSLT errors."))

(defun xslt-error (fmt &rest args)
  (error 'xslt-error :format-control fmt :format-arguments args))

(defun xslt-cerror (fmt &rest args)
  (cerror "recover" 'xslt-error :format-control fmt :format-arguments args))

(defvar *debug* nil)

(defmacro handler-case* (form &rest clauses)
  ;; like HANDLER-CASE if *DEBUG* is off.  If it's on, don't establish
  ;; a handler at all so that we see the real stack traces.  (We could use
  ;; HANDLER-BIND here and check at signalling time, but doesn't seem
  ;; important.)
  (let ((doit (gensym)))
    `(flet ((,doit () ,form))
       (if *debug*
	   (,doit)
	   (handler-case
	       (,doit)
	     ,@clauses)))))

(defun compile-xpath (xpath &optional env)
  (handler-case*
      (xpath:compile-xpath xpath env)
    (xpath:xpath-error (c)
      (xslt-error "~A" c))))


;;;; Helper function and macro

(defun map-pipe-eagerly (fn pipe)
  (xpath::enumerate pipe :key fn :result nil))

(defmacro do-pipe ((var pipe &optional result) &body body)
  `(block nil
     (map-pipe-eagerly #'(lambda (,var) ,@body) ,pipe)
     ,result))


;;;; XSLT-ENVIRONMENT and XSLT-CONTEXT

(defparameter *initial-namespaces*
  '((nil . "")
    ("xmlns" . #"http://www.w3.org/2000/xmlns/")
    ("xml" . #"http://www.w3.org/XML/1998/namespace")))

(defparameter *namespaces* *initial-namespaces*)

(defvar *global-variable-declarations*)
(defvar *lexical-variable-declarations*)

(defvar *global-variable-values*)
(defvar *lexical-variable-values*)

(defclass xslt-environment () ())

(defun split-qname (str)
  (handler-case
      (multiple-value-bind (prefix local-name)
	  (cxml::split-qname str)
	(unless
	    ;; FIXME: cxml should really offer a function that does
	    ;; checks for NCName and QName in a sensible way for user code.
	    ;; cxml::split-qname is tailored to the needs of the parser.
	    ;;
	    ;; For now, let's just check the syntax explicitly.
	    (and (or (null prefix) (xpath::nc-name-p prefix))
		 (xpath::nc-name-p local-name))
	  (xslt-error "not a qname: ~A" str))
	(values prefix local-name))
    (cxml:well-formedness-violation ()
      (xslt-error "not a qname: ~A" str))))

(defun decode-qname (qname env attributep)
  (multiple-value-bind (prefix local-name)
      (split-qname qname)
    (values local-name
	    (if (or prefix (not attributep))
		(xpath:environment-find-namespace env prefix)
		"")
	    prefix)))

(defmethod xpath:environment-find-namespace ((env xslt-environment) prefix)
  (cdr (assoc prefix *namespaces* :test 'equal)))

(defun find-variable-index (local-name uri table)
  (position (cons local-name uri) table :test 'equal))

(defun intern-global-variable (local-name uri)
  (or (find-variable-index local-name uri *global-variable-declarations*)
      (push-variable local-name uri *global-variable-declarations*)))

(defun push-variable (local-name uri table)
  (prog1
      (length table)
    (vector-push-extend (cons local-name uri) table)))

(defun lexical-variable-value (index &optional (errorp t))
  (let ((result (svref *lexical-variable-values* index)))
    (when errorp
      (assert (not (eq result 'unbound))))
    result))

(defun (setf lexical-variable-value) (newval index)
  (assert (not (eq newval 'unbound)))
  (setf (svref *lexical-variable-values* index) newval))

(defun global-variable-value (index &optional (errorp t))
  (let ((result (svref *global-variable-values* index)))
    (when errorp
      (assert (not (eq result 'unbound))))
    result))

(defun (setf global-variable-value) (newval index)
  (assert (not (eq newval 'unbound)))
  (setf (svref *global-variable-values* index) newval))

(defmethod xpath:environment-find-variable
    ((env xslt-environment) lname uri)
  (let ((index
	 (find-variable-index lname uri *lexical-variable-declarations*)))
    (when index
      (lambda (ctx)
	(declare (ignore ctx))
	(svref *lexical-variable-values* index)))))

(defclass lexical-xslt-environment (xslt-environment) ())

(defmethod xpath:environment-find-variable
    ((env lexical-xslt-environment) lname uri)
  (or (call-next-method)
      (let ((index
	     (find-variable-index lname uri *global-variable-declarations*)))
	(when index
	  (lambda (ctx)
	    (declare (ignore ctx))
	    (svref *global-variable-values* index))))))

(defclass global-variable-environment (xslt-environment)
  ((initial-global-variable-thunks
    :initarg :initial-global-variable-thunks
    :accessor initial-global-variable-thunks)))

(defmethod xpath:environment-find-variable
    ((env global-variable-environment) lname uri)
  (or (call-next-method)
      (gethash (cons lname uri) (initial-global-variable-thunks env))))


;;;; TEXT-OUTPUT-SINK
;;;;
;;;; A sink that serializes only text and will error out on any other
;;;; SAX event.

(defmacro with-text-output-sink ((var) &body body)
  `(invoke-with-text-output-sink (lambda (,var) ,@body)))

(defclass text-output-sink (sax:default-handler)
  ((target :initarg :target :accessor text-output-sink-target)))

(defmethod sax:characters ((sink text-output-sink) data)
  (write-string data (text-output-sink-target sink)))

(defun invoke-with-text-output-sink (fn)
  (with-output-to-string (s)
    (funcall fn (make-instance 'text-output-sink :target s))))


;;;; Names

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *xsl* "http://www.w3.org/1999/XSL/Transform")
  (defvar *xml* "http://www.w3.org/XML/1998/namespace")
  (defvar *html* "http://www.w3.org/1999/xhtml"))

(defun of-name (local-name)
  (stp:of-name local-name *xsl*))

(defun namep (node local-name)
  (and (typep node '(or stp:element stp:attribute))
       (equal (stp:namespace-uri node) *xsl*)
       (equal (stp:local-name node) local-name)))


;;;; PARSE-STYLESHEET

(defstruct stylesheet
  (modes (make-hash-table :test 'equal))
  (global-variables ())
  (output-specification (make-output-specification))
  (strip-tests nil)
  (named-templates (make-hash-table :test 'equal)))

(defstruct mode (templates nil))

(defun find-mode (stylesheet local-name &optional uri)
  (gethash (cons local-name uri) (stylesheet-modes stylesheet)))

(defun ensure-mode (stylesheet &optional local-name uri)
  (or (find-mode stylesheet local-name uri)
      (setf (gethash (cons local-name uri) (stylesheet-modes stylesheet))
	    (make-mode))))

(defun ensure-mode/qname (stylesheet qname env)
  (if qname
      (multiple-value-bind (local-name uri)
	  (decode-qname qname env nil)
	(ensure-mode stylesheet local-name uri))
      (find-mode stylesheet nil)))

(defun acons-namespaces (element &optional (bindings *namespaces*))
  (map-namespace-declarations (lambda (prefix uri)
				(push (cons prefix uri) bindings))
			      element)
  bindings)

(defvar *excluded-namespaces* (list *xsl*))
(defvar *empty-mode*)

(defun parse-stylesheet-to-stp (input uri-resolver)
  (let* ((d (cxml:parse input (make-text-normalizer (cxml-stp:make-builder))))
	 (<transform> (stp:document-element d)))
    (strip-stylesheet <transform>)
    ;; FIXME: handle embedded stylesheets
    (unless (and (equal (stp:namespace-uri <transform>) *xsl*)
		 (or (equal (stp:local-name <transform>) "transform")
		     (equal (stp:local-name <transform>) "stylesheet")))
      (xslt-error "not a stylesheet"))
    (dolist (include (stp:filter-children (of-name "include") <transform>))
      (let* ((uri (puri:merge-uris (stp:attribute-value include "href")
				   (stp:base-uri include))))
	(with-open-stream
	    (stream
	     (handler-case
		 (or (and uri-resolver (funcall uri-resolver uri))
		     (open (cxml::uri-to-pathname uri)
			   :element-type '(unsigned-byte 8)))
	       ((or file-error cxml:xml-parse-error) (c)
		 (xslt-error "cannot find included stylesheet ~A: ~A"
			     uri c))))
	  (let ((<transform>2 (parse-stylesheet-to-stp stream uri-resolver)))
	    (stp:do-children (child <transform>2)
	      (stp:insert-child-after <transform>
				      (stp:copy child)
				      include))
	    (stp:detach include)))))
    <transform>))

(defun parse-stylesheet (designator &key uri-resolver)
  (let* ((<transform> (parse-stylesheet-to-stp designator uri-resolver))
	 (*namespaces* (acons-namespaces <transform>))
	 (stylesheet (make-stylesheet))
	 (env (make-instance 'lexical-xslt-environment))
	 (*excluded-namespaces* *excluded-namespaces*)
	 (*global-variable-declarations* (make-empty-declaration-array)))
    (ensure-mode stylesheet nil)
    (parse-exclude-result-prefixes! <transform> env)
    (parse-global-variables! stylesheet <transform>)
    (parse-templates! stylesheet <transform> env)
    (parse-output! stylesheet <transform>)
    (parse-strip/preserve-space! stylesheet <transform> env)
    stylesheet))

(defun parse-exclude-result-prefixes! (<transform> env)
  (stp:with-attributes (exclude-result-prefixes) <transform>
      (dolist (prefix (words (or exclude-result-prefixes "")))
    (when (equal prefix "#default")
      (setf prefix nil))
    (push (or (xpath:environment-find-namespace env prefix)
	      (xslt-error "namespace not found: ~A" prefix))
	  *excluded-namespaces*))))

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
			 (xslt-error "not an NCName: ~A" prefix))
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
  method
  indent
  omit-xml-declaration
  encoding)

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
			      method
			      indent
 			      encoding
;;; 			      media-type
;;; 			      doctype-system
;;; 			      doctype-public
 			      omit-xml-declaration
;;; 			      standalone
;;; 			      cdata-section-elements
			      )
	    <output>
	  (setf (output-method spec) method)
	  (setf (output-indent spec) indent)
	  (setf (output-encoding spec) encoding)
	  (setf (output-omit-xml-declaration spec) omit-xml-declaration))))))

(defun make-empty-declaration-array ()
  (make-array 1 :fill-pointer 0 :adjustable t))

(defun make-variable-value-array (n-lexical-variables)
  (make-array n-lexical-variables :initial-element 'unbound))

(defun compile-global-variable (<variable> env) ;; also for <param>
  (stp:with-attributes (name select) <variable>
    (when (and select (stp:list-children <variable>))
      (xslt-error "variable with select and body"))
    (let* ((*lexical-variable-declarations* (make-empty-declaration-array))
	   (inner (cond
		    (select
		     (compile-xpath select env))
		    ((stp:list-children <variable>)
		     (let* ((inner-sexpr `(progn ,@(parse-body <variable>)))
			    (inner-thunk (compile-instruction inner-sexpr env)))
		       (lambda (ctx)
			 (apply-to-result-tree-fragment ctx inner-thunk))))
		    (t
		     (lambda (ctx)
		       (declare (ignore ctx))
		       ""))))
	   (n-lexical-variables (length *lexical-variable-declarations*)))
      (lambda (ctx)
	(let ((*lexical-variable-values*
	       (make-variable-value-array n-lexical-variables)))
	  (funcall inner ctx))))))

(defstruct (variable-information
	     (:constructor make-variable)
	     (:conc-name "VARIABLE-"))
  index
  thunk
  local-name
  uri
  param-p
  thunk-setter)

(defun parse-global-variable! (<variable> global-env) ;; also for <param>
  (let ((*namespaces* (acons-namespaces <variable>))
	(qname (stp:attribute-value <variable> "name")))
    (unless qname
      (xslt-error "name missing in ~A" (stp:local-name <variable>)))
    (multiple-value-bind (local-name uri)
	(decode-qname qname global-env nil)
      ;; For the normal compilation environment of templates, install it
      ;; into *GLOBAL-VARIABLE-DECLARATIONS*:
      (let ((index (intern-global-variable local-name uri)))
        ;; For the evaluation of a global variable itself, build a thunk
        ;; that lazily resolves other variables, stored into
	;; INITIAL-GLOBAL-VARIABLE-THUNKS:
        (let* ((value-thunk :unknown)
               (global-variable-thunk
                (lambda (ctx)
                  (let ((v (global-variable-value index nil)))
		    (when (eq v 'seen)
		      (xslt-error "recursive variable definition"))
		    (cond
		      ((eq v 'unbound)
		       ;; (print (list :computing index))
		       (setf (global-variable-value index) 'seen)
		       (setf (global-variable-value index)
			     (funcall value-thunk ctx))
		       #+nil (print (list :done-computing index
				    (global-variable-value index)))
		       #+nil (global-variable-value index))
		      (t
		       #+nil(print (list :have
				    index v))
		       v)))))
               (thunk-setter
                (lambda ()
                  (setf value-thunk
                        (compile-global-variable <variable> global-env)))))
          (setf (gethash (cons local-name uri)
			 (initial-global-variable-thunks global-env))
                global-variable-thunk)
          (make-variable :index index
                         :local-name local-name
                         :uri uri
                         :thunk global-variable-thunk
                         :param-p (namep <variable> "param")
                         :thunk-setter thunk-setter))))))

(xpath:with-namespaces ((nil #.*xsl*))
  (defun parse-global-variables! (stylesheet <transform>)
    (let* ((table (make-hash-table :test 'equal))
	   (global-env (make-instance 'global-variable-environment
				      :initial-global-variable-thunks table))
	   (specs '()))
      (xpath:do-node-set
	  (<variable> (xpath:evaluate "variable|param" <transform>))
	(let ((var (parse-global-variable! <variable> global-env)))
	  (when (find var
		      specs
		      :test (lambda (a b)
			      (and (equal (variable-local-name a)
					  (variable-local-name b))
				   (equal (variable-uri a)
					  (variable-uri b)))))
	    (xslt-error "duplicate definition for global variable ~A"
			(variable-local-name var)))
	  (push var specs)))
      ;; now that the global environment knows about all variables, run the
      ;; thunk setters to perform their compilation
      (mapc (lambda (spec) (funcall (variable-thunk-setter spec))) specs)
      (setf (stylesheet-global-variables stylesheet) specs))))

(defun parse-templates! (stylesheet <transform> env)
  (dolist (<template> (stp:filter-children (of-name "template") <transform>))
    (let ((*namespaces* (acons-namespaces <template>)))
      (dolist (template (compile-template <template> env))
        (if (template-name template)
	    (setf (gethash (template-name template)
			   (stylesheet-named-templates stylesheet))
		  template)
	    (let ((mode (ensure-mode/qname stylesheet
					   (template-mode-qname template)
					   env)))
	      (setf (template-mode template) mode)
	      (push template (mode-templates mode))))))))


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
     (handler-case*
	 (let* ((*stylesheet* stylesheet)
		(*mode* (find-mode stylesheet nil))
		(*empty-mode* (make-mode))
		(global-variable-specs
		 (stylesheet-global-variables stylesheet))
		(*global-variable-values*
		 (make-variable-value-array (length global-variable-specs)))
		(ctx (xpath:make-context
		      (make-whitespace-stripper
		       source-document
		       (stylesheet-strip-tests stylesheet)))))
	   (mapc (lambda (spec)
		   (when (variable-param-p spec)
		     (let ((value
			    (find-parameter-value (variable-local-name spec)
						  (variable-uri spec)
						  parameters)))
		       (when value
			 (setf (global-variable-value (variable-index spec))
			       value)))))
		 global-variable-specs)
	   (mapc (lambda (spec)
		   (funcall (variable-thunk spec) ctx))
		 global-variable-specs)
	   #+nil (print global-variable-specs)
	   #+nil (print *global-variable-values*)
	   (apply-templates ctx))
       (xpath:xpath-error (c)
			  (xslt-error "~A" c))))
   stylesheet
   output))

(defun apply-templates/list (list &optional param-bindings)
  (let* ((n (length list))
	 (s/d (lambda () n)))
    (loop
       for i from 1
       for child in list
       do
	 (apply-templates (xpath:make-context child s/d i)
           param-bindings))))

(defun invoke-template (ctx template param-bindings)
  (let ((*lexical-variable-values*
	 (make-variable-value-array (template-n-variables template))))
    (loop
       for (name-cons value) in param-bindings
       for (nil index nil) = (find name-cons
				   (template-params template)
				   :test #'equal
				   :key #'car)
       do
	 (unless index
	   (xslt-error "invalid template parameter ~A" name-cons))
	 (setf (lexical-variable-value index) value))
    (funcall (template-body template) ctx)))

(defun apply-templates (ctx &optional param-bindings)
  (let ((template (find-template ctx)))
    (if template
	(invoke-template ctx template param-bindings)
	(let ((node (xpath:context-node ctx)))
	  (cond
	    ((or (xpath-protocol:node-type-p node :processing-instruction)
		 (xpath-protocol:node-type-p node :comment)))
	    ((or (xpath-protocol:node-type-p node :text)
		 (xpath-protocol:node-type-p node :attribute))
	     (write-text (xpath-protocol:string-value node)))
	    (t
	     (apply-templates/list
	      (xpath::force
	       (xpath-protocol:child-pipe node)))))))))

(defun call-template (ctx name &optional param-bindings)
  (invoke-template ctx (find-named-template name) param-bindings))

(defun find-template (ctx)
  (let* ((matching-candidates
	  (remove-if-not (lambda (template)
			   (template-matches-p template ctx))
			 (mode-templates *mode*))))
    (maximize #'template< matching-candidates)))

(defun find-named-template (name)
  (or (gethash name (stylesheet-named-templates *stylesheet*))
      (error "cannot find named template: ~s" name)))

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
	  (xslt-error "conflicting templates: ~A, ~A" a b)))))))

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
     (with-xml-output output
       (funcall fn)))))

(defun make-output-sink (stylesheet stream)
  (let* ((ystream
	  (if stream
	      (let ((et (stream-element-type stream)))
		(cond
		  ((or (null et) (subtypep et '(unsigned-byte 8)))
		   (runes:make-octet-stream-ystream stream))
		  ((subtypep et 'character)
		   (runes:make-character-stream-ystream stream))))
	      (runes:make-rod-ystream)))
	 (output-spec (stylesheet-output-specification stylesheet))
	 (omit-xml-declaration-p
	  (equal (output-omit-xml-declaration output-spec) "yes"))
	 (sax-target
	  (make-instance 'cxml::sink
			 :ystream ystream
			 :omit-xml-declaration-p omit-xml-declaration-p)))
    (if (equalp (output-method (stylesheet-output-specification stylesheet))
		"HTML")
	(make-instance 'combi-sink
		       :hax-target (make-instance 'chtml::sink
						  :ystream ystream)
		       :sax-target sax-target
		       :encoding (output-encoding output-spec))
	sax-target)))

(defstruct template
  match-expression
  match-thunk
  name
  priority
  mode
  mode-qname
  params
  body
  n-variables)

(defun template-import-precedence (template)
  template
  ;; fixme
  0)

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
      (xslt-error "not a valid pattern: ~A" str))
    (mapcar (lambda (case)
	      (unless (eq (car case) :path) ;zzz: filter statt path
		(xslt-error "not a valid pattern: ~A" str))
	      `(:path (:ancestor-or-self :node) ,@(cdr case)))
	    (if (eq (car form) :union)
		(cdr form)
		(list form)))))

(defun compile-value-thunk (value env)
  (if (and (listp value) (eq (car value) 'progn))
      (let ((inner-thunk (compile-instruction value env)))
        (lambda (ctx)
          (apply-to-result-tree-fragment ctx inner-thunk)))
      (compile-xpath value env)))

(defun compile-var-bindings/nointern (forms env)
  (loop
    for (name value) in forms
    collect (multiple-value-bind (local-name uri)
                (decode-qname name env nil)
              (list (cons local-name uri)
                    (compile-value-thunk value env)))))

(defun compile-var-bindings (forms env)
  (loop
     for (cons thunk) in (compile-var-bindings/nointern forms env)
     for (local-name . uri) = cons
     collect (list cons
		   (push-variable local-name
				  uri
				  *lexical-variable-declarations*)
		   thunk)))

(defun compile-template (<template> env)
  (stp:with-attributes (match name priority mode) <template>
    (unless (or name match)
      (xslt-error "missing match in template"))
    (multiple-value-bind (params body-pos)
        (loop
          for i from 0
          for child in (stp:list-children <template>)
          while (namep child "param")
          collect (parse-param child) into params
          finally (return (values params i)))
      (let* ((*lexical-variable-declarations* (make-empty-declaration-array))
             (param-bindings (compile-var-bindings params env))
	     (body (parse-body <template> body-pos (mapcar #'car params)))
             (body-thunk (compile-instruction `(progn ,@body) env))
             (outer-body-thunk
              #'(lambda (ctx)
                  ;; set params that weren't initialized by apply-templates
                  (loop for (name index param-thunk) in param-bindings
                        when (eq (lexical-variable-value index nil) 'unbound)
                          do (setf (lexical-variable-value index)
                                   (funcall param-thunk ctx)))
                  (funcall body-thunk ctx)))
	     (n-variables (length *lexical-variable-declarations*)))
        (append
         (when name
           (multiple-value-bind (local-name uri)
               (decode-qname name env nil)
             (list
              (make-template :name (cons local-name uri)
                             :params param-bindings
                             :body outer-body-thunk
			     :n-variables n-variables))))
         (when match
           (mapcar (lambda (expression)
                     (let ((match-thunk
                            (compile-xpath `(xpath:xpath ,expression) env))
                           (p (if priority
                                  (parse-number:parse-number priority)
                                  (expression-priority expression))))
                       (make-template :match-expression expression
                                      :match-thunk match-thunk
                                      :priority p
                                      :mode-qname mode
                                      :params param-bindings
                                      :body outer-body-thunk
				      :n-variables n-variables)))
                   (parse-pattern match))))))))
#+(or)
(xuriella::parse-stylesheet #p"/home/david/src/lisp/xuriella/test.xsl")
