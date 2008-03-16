;;; -*- show-trailing-whitespace: t; indent-tabs-mode: nil -*-

;;; Copyright (c) 2007,2008 David Lichteblau, Ivan Shvedunov.
;;; Copyright (c) 2004 David Lichteblau (for headcraft.de)
;;; All rights reserved.

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


;;; Convenience functions for serialization to SAX, similar in syntax
;;; to what cxml offers, but with namespace handling as required for XSLT.

(defvar *current-element*)
(defvar *sink*)
(defvar *start-tag-written-p*)

(defmacro with-xml-output (sink &body body)
  `(invoke-with-xml-output (lambda () ,@body) ,sink))

(defmacro with-output-sink-bound ((var) &body body)
  `(invoke-with-output-sink-bound (lambda (,var) ,@body)))

(defun invoke-with-xml-output (fn sink)
  (let ((*sink* sink)
        (*current-element* nil)
        (*start-tag-written-p* t))
    (sax:start-document *sink*)
    (funcall fn)
    (sax:end-document *sink*)))

(defun invoke-with-output-sink-bound (fn)
  (maybe-emit-start-tag)
  (funcall fn *sink*))

(defmacro with-element
    ((local-name uri &key suggested-prefix extra-namespaces process-aliases)
     &body body)
  `(invoke-with-element (lambda () ,@body)
                        ,local-name
                        ,uri
                        :suggested-prefix ,suggested-prefix
                        :extra-namespaces ,extra-namespaces
                        :process-aliases ,process-aliases))

(defun doctype (name public-id system-id &optional internal-subset)
  (sax:start-dtd *sink* name public-id system-id)
  (when internal-subset
    (sax:unparsed-internal-subset *sink* internal-subset))
  (sax:end-dtd *sink*))

(defun maybe-emit-start-tag ()
  (let ((elt *current-element*))
    (when (and elt (not *start-tag-written-p*))
      (setf *start-tag-written-p* t)
      (let* ((local-name (sink-element-local-name elt))
             (uri (sink-element-uri elt))
             (suggested-prefix (sink-element-suggested-prefix elt))
             (prefix (ensure-prefix-for-uri elt uri suggested-prefix))
             (qname (if (plusp (length prefix))
                        (concatenate 'string prefix ":" local-name)
                        local-name))
             (attrs '()))
        (setf (sink-element-actual-qname elt) qname)
        (dolist (attr (sink-element-attributes elt))
          (push (convert-attribute elt attr) attrs))
        (loop
           for (prefix . uri) in (sink-element-new-namespaces elt) do
             (sax:start-prefix-mapping *sink* prefix uri)
             (push (make-xmlns-attribute prefix uri) attrs))
        (sax:start-element *sink* uri local-name qname attrs)))))

(defun convert-attribute (elt attr)
  (let* ((local-name (sink-attribute-local-name attr))
         (uri (sink-attribute-uri attr))
         (suggested-prefix (sink-attribute-suggested-prefix attr))
         (prefix (ensure-prefix-for-uri elt uri suggested-prefix t))
         (qname (if (plusp (length prefix))
                    (concatenate 'string prefix ":" local-name)
                    local-name)))
    (sax:make-attribute :namespace-uri uri
                        :local-name local-name
                        :qname qname
                        :value (sink-attribute-value attr))))

(defun sink-element-find-uri (prefix elt)
  (assert prefix)
  (cdr
   (find prefix
         (sink-element-all-namespaces elt)
         :key #'car
         :test #'equal)))

(defun ensure-prefix-for-uri (elt uri suggested-prefix &optional attributep)
  (check-type uri string)
  (when (or (equal suggested-prefix "xmlns")
            (equal suggested-prefix "xml"))
    (setf suggested-prefix nil))
  (let* ((prefix-cons
          (find uri
                (sink-element-all-namespaces elt)
                :key #'cdr
                :test #'equal))
         (prefix (car prefix-cons))
         (cross-check
          (when prefix-cons
            (sink-element-find-uri prefix elt))))
    (cond
      ((equal uri "")
       (unless (or attributep
                   (equal (sink-element-find-uri "" elt) ""))
         (push-sink-element-namespace elt "" ""))
       "")
      ((and prefix-cons
            (equal cross-check uri)
            (or (plusp (length prefix))
                (not attributep)))
       (pushnew prefix (sink-element-used-prefixes elt) :test #'equal)
       prefix)
      ((and (or (plusp (length suggested-prefix))
                (not attributep))
            (not (find suggested-prefix
                       (sink-element-new-namespaces elt)
                       :key #'car
                       :test #'equal))
            (not (find suggested-prefix
                       (sink-element-used-prefixes elt)
                       :test #'equal)))
       (push-sink-element-namespace elt (or suggested-prefix "") uri)
       suggested-prefix)
      (t
       (loop
           for i from 0
           for prefix = (format nil "ns-~D" i)
           while (sink-element-find-uri prefix elt)
           finally
	     (push-sink-element-namespace elt prefix uri)
             (return prefix))))))

(defun make-xmlns-attribute (prefix uri)
  (sax:make-attribute
   :namespace-uri #"http://www.w3.org/2000/xmlns/"
   :local-name prefix
   :qname (if (zerop (length prefix))
              "xmlns"
              (concatenate 'string "xmlns:" prefix))
   :value uri))

(defstruct sink-element
  local-name
  uri
  suggested-prefix
  all-namespaces
  new-namespaces
  used-prefixes
  attributes
  actual-qname)

(defstruct sink-attribute
  local-name
  uri
  suggested-prefix
  value)

(defparameter *initial-namespaces*
  '(("" . "")
    ("xmlns" . #"http://www.w3.org/2000/xmlns/")
    ("xml" . #"http://www.w3.org/XML/1998/namespace")))

(defun invoke-with-element
    (fn local-name uri &key suggested-prefix extra-namespaces process-aliases)
  (check-type local-name string)
  (check-type uri string)
  (check-type suggested-prefix (or null string))
  (maybe-emit-start-tag)
  (when process-aliases
    (setf uri (unalias-uri uri)))
  (let* ((parent *current-element*)
         (elt (make-sink-element
               :local-name local-name
               :uri uri
               :suggested-prefix suggested-prefix
               :all-namespaces (if parent
                                   (sink-element-all-namespaces parent)
                                   *initial-namespaces*)
               :new-namespaces nil
               :attributes nil))
         (*current-element* elt)
         (*start-tag-written-p* nil))
    ;; always establish explicitly copied namespaces first
    ;; (not including declarations of the default namespace)
    (process-extra-namespaces elt extra-namespaces process-aliases)
    ;; establish the element's prefix (which might have to be the default
    ;; namespace if it's the empty URI)
    (ensure-prefix-for-uri elt uri suggested-prefix)
    ;; we'll do attributes incrementally
    (multiple-value-prog1
        (funcall fn)
      (maybe-emit-start-tag)
      (sax:end-element *sink* uri local-name (sink-element-actual-qname elt))
      (loop
         for (prefix . uri) in (sink-element-new-namespaces elt) do
         (sax:end-prefix-mapping *sink* prefix)))))

(defun process-extra-namespace (elt prefix uri process-aliases)
  (when process-aliases
    (setf uri (unalias-uri uri)))
  (unless
      (or
       ;; don't touch the empty prefix, since we might need it for the empty
       ;; URI
       (zerop (length prefix))
       ;; don't touch the empty URI
       (zerop (length uri))
       ;; allow earlier conses in extra-namespaces to hide later ones.
       ;; FIXME: add a good explanation here why we need to do this both
       ;; here and in remove-extra-namespaces.
       (find prefix
             (sink-element-new-namespaces elt)
             :key #'car
             :test #'equal))
    (let ((previous (sink-element-find-uri prefix elt)))
      (if (equal uri previous) ;no need to declare what has already been done
          (pushnew prefix (sink-element-used-prefixes elt) :test #'equal)
          (push-sink-element-namespace elt prefix uri))))))

(defun process-extra-namespaces (elt extra-namespaces process-aliases)
  (loop for (prefix . uri) in extra-namespaces do
       (process-extra-namespace elt prefix uri process-aliases)))

(defun push-sink-element-namespace (elt prefix uri)
  (assert prefix)
  (cond
    ((equal prefix "xml")
     (assert (equal uri "http://www.w3.org/XML/1998/namespace")))
    ((equal prefix "xmlns")
     (assert (equal uri "http://www.w3.org/2000/xmlns/")))
    (t
     (let ((cons (cons prefix uri)))
       (push cons (sink-element-all-namespaces elt))
       (push cons (sink-element-new-namespaces elt))))))

(defun write-attribute
    (local-name uri value &key suggested-prefix process-aliases)
  (check-type local-name string)
  (check-type uri string)
  (check-type value string)
  (check-type suggested-prefix (or null string))
  (when process-aliases
    (setf uri (unalias-uri uri)))
  (cond
    ((null *current-element*)
     (xslt-error "attribute outside of element"))
    (*start-tag-written-p*
     (xslt-cerror "attribute after start tag"))
    ((equal local-name "xmlns")
     (xslt-error "attribute named xmlns"))
    (t
     (setf (sink-element-attributes *current-element*)
           (cons (make-sink-attribute :local-name local-name
                                      :uri uri
                                      :suggested-prefix suggested-prefix
                                      :value value)
                 (delete-if (lambda (x)
                              (and (equal (sink-attribute-local-name x)
                                          local-name)
                                   (equal (sink-attribute-uri x) uri)))
                            (sink-element-attributes *current-element*)))))))

(defun write-extra-namespace (prefix uri process-aliases)
  (check-type prefix string)
  (check-type uri string)
  (cond
    ((null *current-element*)
     (xslt-error "attribute outside of element"))
    (*start-tag-written-p*
     (xslt-cerror "namespace after start tag"))
    ((zerop (length prefix))
     (xslt-cerror "refusing to copy declaration for default namespace"))
    (t
     (process-extra-namespace *current-element* prefix uri process-aliases))))

(defun write-text (data)
  (maybe-emit-start-tag)
  (sax:characters *sink* data)
  data)

(defun write-comment (data)
  (maybe-emit-start-tag)
  (sax:comment *sink* data)
  data)

(defun write-processing-instruction (target data)
  (maybe-emit-start-tag)
  (sax:processing-instruction *sink* target data)
  data)

(defun write-unescaped (str)
  (maybe-emit-start-tag)
  (sax:unescaped *sink* str))
