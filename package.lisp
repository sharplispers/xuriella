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

(defpackage :xuriella
  (:use :cl)
  (:export #:parse-stylesheet
           #:apply-stylesheet
           #:make-parameter)
  (:import-from :xpath-protocol #:define-default-method)
  (:documentation
   "Empty is an empty example.

    @begin[Empty section]{section}
    I am a blind text.

    @end{section}"))
