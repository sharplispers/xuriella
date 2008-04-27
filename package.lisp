(defpackage :xsl
  (:nicknames :xslt)
  (:use)
  (:export #:template
           #:apply-templates
           #:apply-imports
           #:call-template
           #:element
           #:literal-element
           #:literal-attribute
           #:attribute
           #:text
           #:unescaped-text
           #:processing-instruction
           #:comment
           #:copy
           #:value-of
           #:unescaped-value-of
           #:number
           #:for-each
           #:with-base-uri
           #:copy-of
           #:message
           #:terminate
           #:fallback
           #:use-attribute-sets

	   ;; Extensions
           #:document			;fixme: incompatible with XSLT 2.0

	   ;; xuriella internals
	   #:with-version
           #:with-namespaces
           #:with-excluded-namespaces
           #:with-extension-namespaces
           #:with-duplicates-check))

(defpackage :xuriella-extensions
  (:use)
  (:export #:document))

(defpackage :xuriella
  (:use :cl)
  (:export #:parse-stylesheet
           #:apply-stylesheet
           #:make-parameter

	   #:define-extension-group
	   #:define-extension-parser
	   #:define-extension-compiler
	   #:parse-body
	   #:compile-instruction)
  (:import-from :xpath-protocol #:define-default-method)
  (:documentation
   "Xuriella is an implementation of XSLT 1.0."))
