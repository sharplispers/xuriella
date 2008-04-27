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
   "Xuriella is an implementation of XSLT 1.0.

    @begin[Using XSLT]{section}
      XSLT stylesheets are invoked using the @code{apply-stylesheet} function,
      which can parse, compile, and apply XSLT stylesheets.

      Top-level parameters to the stylesheet can be specified using
      parameter instances created by @fun{make-parameter}.

      @aboutfun{apply-stylesheet}
      @aboutfun{make-parameter}
    @end{section}
    @begin[Compiling stylesheets explicitly]{section}
      @code{parse-stylesheet} allows the compilation of XSLT stylesheets into
      objects ahead of time, so that @code{apply-stylesheet} only needs to
      invoke the pre-compiled sheet rather than having to parse and
      compile it first.

      @aboutfun{parse-stylesheet}
    @end{section}
    @begin[Defining extension elements]{section}
      Xuriella can be extended in two ways:

      Custom XPath functions can be implemented using the extension
      mechanism in @a[http://common-lisp.net/project/plexippus-xpath/atdoc/pages/xpath-sys.html]{Plexippus}.

      Custom XSLT elements can be implemented using the following macros.

      @code{define-extension-group} is used to establish a namespace for
      the extensions, which can then be activated using a namespace declaration
      and the @code{extension-element-prefixes} attribute in the stylesheet.

      Every individual extension element needs at least a definition
      using @code{define-extension-parser}.  The parser will run at
      compilation time and return an XSLT instruction in a sexp syntax.
      If the extension can be implemented as a transformation into ordinary
      XSLT elements, the parser only needs to return that XSLT sexp.

      In addition, the sexp representation itself can be extended using
      @code{define-extension-compiler}.  The extension compiler will be
      invoked while the stylesheet is compiled to return a function, usually
      a closure, that will be called by the stylesheet at run-time.

      @aboutmacro{define-extension-group}
      @aboutmacro{define-extension-parser}
      @aboutmacro{define-extension-compiler}
    @end{section}
    @begin[Functions useful in extensions]{section}
      The following functions can be used by extension parsers and compilers,
      to parse child nodes as instructions, or to compile such instructions,
      respectively.

      @aboutfun{parse-body}
      @aboutfun{compile-instruction}
    @end{section}"))
