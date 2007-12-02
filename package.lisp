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
	   ;; #:number
	   #:for-each
	   #:with-namespaces
	   #:copy-of
	   #:message
	   #:terminate
	   #:fallback))

(defpackage :xuriella
  (:use :cl)
  (:export #:parse-stylesheet
	   #:apply-stylesheet
	   #:make-parameter)
  (:documentation
   "Empty is an empty example.

    @begin[Empty section]{section}
    I am a blind text.

    @end{section}"))
