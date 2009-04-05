(asdf:operate 'asdf:load-op :xuriella)
(asdf:operate 'asdf:load-op :atdoc)
(let* ((base (asdf:component-pathname (asdf:find-system :xuriella)))
       (atdoc-directory (merge-pathnames "doc/atdoc/" base)))
  (ensure-directories-exist atdoc-directory)
  (atdoc:generate-html-documentation
   '(:xuriella)
   atdoc-directory
   :index-title "Xuriella XSLT API reference"
   :heading "Xuriella XSLT"
   :css (merge-pathnames "doc/atdoc.css" base)
   :single-page-p t))
