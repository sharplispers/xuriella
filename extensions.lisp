;;; -*- show-trailing-whitespace: t; indent-tabs-mode: nil -*-

;;; Copyright (c) 2007,2008 Ivan Shvedunov. All rights reserved.
;;; Copyright (c) 2007,2008 David Lichteblau. All rights reserved.

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


;;;; XSLT extensions

(defvar *extension-groups* (make-hash-table :test #'equal))

(defstruct extension-group
  uri
  documentation
  (elements (make-hash-table :test #'equal)))

(defstruct extension-element
  local-name
  (parser (lambda (&rest ignore)
            (declare (ignore ignore))
            (xslt-error "extension parser not defined"))))

(defun %define-extension-group (name uri documentation)
  (check-type uri string)
  (let* ((current-ext (get name 'extension-group))
         (new-ext
          (cond (current-ext
                 (setf (gethash (extension-group-uri current-ext)
                                *extension-groups*)
                       (remove current-ext
                               (gethash (extension-group-uri current-ext)
                                        *extension-groups*))
                       (extension-group-uri current-ext) uri
                       (extension-group-documentation current-ext) documentation)
                 current-ext)
                (t
                 (setf (get name 'extension-group)
                       (make-extension-group :uri uri
                                             :documentation documentation))))))
    (push new-ext (gethash uri *extension-groups*))))

(defmacro define-extension-group (name uri &optional documentation)
  (check-type name symbol)
  `(%define-extension-group ',name ,uri ,documentation))

(defun find-extension-element (local-name uri)
  (loop for ext in (gethash uri *extension-groups*)
        for match = (gethash local-name (extension-group-elements ext))
        when match
          do (return match)))

(defun ensure-extension-element (ext name)
  (check-type name string)
  (setf (gethash name
                 (extension-group-elements
                  (or (get ext 'extension-group)
                      (error "no such extension: ~s" ext))))
        (make-extension-element :local-name name)))

(defmacro define-extension-parser (ext name (node-var) &body body)
  `(setf (extension-element-parser
          (ensure-extension-element ',ext ',name))
         (lambda (,node-var)
           ,@body)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun parse-extension-lambda-list (lambda-list)
    ;; find &environment
    (loop
       for (form . rest) on lambda-list
       when (eq form '&environment)
       do
         (destructuring-bind (env-var &rest rest-rest) rest
           (check-type env-var (and symbol (not null)))
           (when (find '&environment rest-rest)
             (error "duplicate &environment in extension lambda list"))
           (return
             (values env-var (append normal-forms rest-rest))))
       collect form into normal-forms
       finally
         (return (values 'ignore normal-forms)))))

(defmacro define-extension-compiler (symbol (&rest lambda-list) &body body)
  (when (find (symbol-package symbol)
              ;; reserved for built-in instructions:
              (list (find-package :common-lisp)
                    (find-package :xslt)
                    (find-package :xuriella)))
    (error "cannot define XSLT extensions in the ~A package"
           (symbol-package symbol)))
  (multiple-value-bind (env argument-lambda-list)
      (parse-extension-lambda-list lambda-list)
    (let ((args (gensym)))
      `(setf (get ',symbol 'extension-compiler)
             (lambda (,ARGS ,env)
               (declare (ignorable ,env))
               (destructuring-bind (,@argument-lambda-list) ,ARGS
                 ,@body))))))



;;;; our <document> extension

(define-extension-group :xuriella "http://common-lisp.net/project/xuriella"
  "XSLT extensions provided by Xuriella.")

(define-extension-parser :xuriella "document" (node)
  (only-with-attributes
   (href method indent doctype-public doctype-system) node
    `(xuriella-extensions:document
      (,href :method ,method
             :indent ,indent
             :doctype-public ,doctype-public
             :doctype-system ,doctype-system)
      ,@(parse-body node))))

(define-extension-compiler xuriella-extensions:document
    ((href &key method indent doctype-public doctype-system)
     &body body
     &environment env)
  (declare (ignore doctype-public doctype-system)) ;fixme
  (let ((thunk (compile-instruction `(progn ,@body) env))
        (href-thunk (compile-avt href env)))
    (lambda (ctx)
      (let ((pathname
             (uri-to-pathname
              (puri:merge-uris (funcall href-thunk ctx)
                               (xpath-protocol:base-uri
                                (xpath:context-node ctx))))))
        (ensure-directories-exist pathname) ;really?
        (invoke-with-output-sink
         (lambda ()
           (funcall thunk ctx))
         (make-output-specification
          :method (cond
                    ((or (null method) (equalp method "XML")) :xml)
                    ((equalp method "HTML") :html)
                    ((equalp method "TEXT") :text)
                    (t
                     (xslt-error "invalid output method: ~A" method)))
          :indent indent)
         pathname)))))
