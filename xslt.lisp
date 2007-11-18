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


;;;; XSLT-ENVIRONMENT and XSLT-CONTEXT

(defparameter *namespaces* cxml::*initial-namespace-bindings*)
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

(defmethod xpath:context-variable-value ((context xslt-context) local-name uri)
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
