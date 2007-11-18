(defpackage :xsl
  (:nicknames :xslt)
  (:use :cl)
  (:export #:template
	   #:apply-templates
	   #:apply-imports
	   #:call-template
	   #:element
	   #:attibute
	   #:text
	   #:unescaped-text
	   #:processing-instruction
	   #:comment
	   #:copy
	   #:value-of
	   #:unescaped-value-of
	   #:number
	   #:for-each
	   #:when
	   #:if
	   #:progn
	   #:cond
	   #:let
	   #:let*
	   #:with-namespaces
	   #:copy-of
	   #:message
	   #:terminate
	   #:fallback))

(defpackage :xuriella
  (:use :cl :xsl)
  (:export )
  (:documentation
   "Empty is an empty example.

    @begin[Empty section]{section}
    I am a blind text.

    @end{section}"))
