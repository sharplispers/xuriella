;;; -*- show-trailing-whitespace: t; indent-tabs-mode: nil -*-

;;; Copyright (c) 2007,2008 David Lichteblau, Ivan Shvedunov.
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


;;; This file implements whitespace stripping.
;;;
;;; Although the spec presents a unified algorithm for whitespace stripping
;;; of stylesheets and source documents, we implement them separately.
;;;
;;; For stylesheets, the STP parse tree of the stylesheet is modified
;;; directly according the its xml:space declarations and xsl:text elements.
;;;
;;; For source documents, the strip-space and preserve-space declarations
;;; from the stylesheet are taken into account.  To avoid processing
;;; parts of the document that XPath would not otherwise have navigated
;;; to, we do whitespace stripping lazily using a proxy implementation
;;; of the XPath protocol.

(in-package :xuriella)

#+sbcl
(declaim (optimize (debug 2)))


;;;; Helper functions

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *whitespace*
    (format nil "~C~C~C~C"
            (code-char 9)
            (code-char 32)
            (code-char 13)
            (code-char 10))))

(defun normalize-whitespace (str)
  (cl-ppcre:regex-replace-all #.(format nil "[~A]+" *whitespace*)
                              (string-trim *whitespace* str)
                              " "))

(defun whitespacep (str)
  (cl-ppcre:all-matches #.(format nil "^[~A]+$" *whitespace*) str))

(defun words (str)
  (when str
    (cl-ppcre:split #.(format nil "[~A]+" *whitespace*)
                    (string-trim *whitespace* str))))


;;;; Strip whitespace in stylesheets

;; Also strips comments and PIs.
(defun strip-stylesheet (parent &optional preserve)
  (let ((i 0))
    (loop while (< i (length (cxml-stp-impl::%children parent))) do
         (let ((child (stp:nth-child i parent)))
           (etypecase child
             (stp:text
              (if (and (whitespacep (stp:data child))
                       (not preserve))
                  (stp:delete-nth-child i parent)
                  (incf i)))
             ((or stp:comment stp:processing-instruction)
              (stp:delete-nth-child i parent))
             (stp:element
                 (stp:with-attributes ((space "space" *xml*))
                     child
                   (let ((new-preserve
                          (cond
                            ((namep child "text") t)
                            ((not space) preserve)
                            ((equal space "preserve") t)
                            (t nil))))
                     (strip-stylesheet child new-preserve)))
               (incf i)))))))


;;;; Strip whitespace in source documents

(defun make-whitespace-stripper (node tests)
  (if tests
      (make-stripping-node nil node tests nil)
      node))

(defstruct (stripping-node (:constructor #:ignore))
  parent
  target
  children)

(defstruct (leaf-stripping-node
             (:constructor make-leaf-stripping-node (parent target))
             (:include stripping-node)))

(defstruct (parent-stripping-node
             (:constructor make-parent-stripping-node (parent target))
             (:include stripping-node)))

(defmethod print-object ((object stripping-node) stream)
  (print-unreadable-object (object stream :type t :identity nil)
    (let ((target (write-to-string (stripping-node-target object))))
      (if (and (alexandria:starts-with-subseq target "#<")
               (alexandria:ends-with #\> target))
          (write-sequence target stream :start 3 :end (1- (length target)))
          (write-sequence target stream)))))

(defun strip-under-qname-p (node tests)
  (let ((local-name (xpath-protocol:local-name node))
        (uri (xpath-protocol:namespace-uri node)))
    (dolist (test tests nil)
      (let ((result (funcall test local-name uri)))
        (when result
          (return (eq result :strip)))))))

(defun xpath-protocol/attribute-value (node local-name uri)
  (do-pipe (a (xpath-protocol:attribute-pipe node))
    (when (and (equal (xpath-protocol:local-name a) local-name)
               (equal (xpath-protocol:namespace-uri a) uri))
      (return (xpath-protocol:node-text a)))))

(defun make-stripping-node (parent target tests force-preserve)
  (let ((result (make-parent-stripping-node parent target))
        (xml-space (xpath-protocol/attribute-value target "space" *xml*)))
    (when xml-space
      (setf force-preserve (equal xml-space "preserve")))
    (labels ((recurse (child-node)
               (if (xpath-protocol:node-type-p child-node :element)
                   (make-stripping-node result child-node tests force-preserve)
                   (make-leaf-stripping-node result child-node)))
             (maybe-recurse (child-node)
               (if (and (xpath-protocol:node-type-p child-node :text)
                        (whitespacep (xpath-protocol:node-text child-node)))
                   nil
                   (recurse child-node))))
      (let ((all-children (xpath-protocol:child-pipe target)))
        (setf (stripping-node-children result)
              (if (or force-preserve
                      (not (xpath-protocol:node-type-p target :element))
                      (not (strip-under-qname-p target tests)))
                  (xpath::map-pipe-filtering #'recurse all-children)
                  (xpath::map-pipe-filtering #'maybe-recurse all-children)))))
    result))

(macrolet ((defproxy (name &rest args)
             `(define-default-method ,name ((node stripping-node) ,@args)
                (,name (stripping-node-target node) ,@args))))
  (defproxy xpath-protocol:local-name)
  (defproxy xpath-protocol:namespace-uri)
  (defproxy xpath-protocol:namespace-prefix)
  (defproxy xpath-protocol:qualified-name)
  (defproxy xpath-protocol:attribute-pipe)
  (defproxy xpath-protocol:namespace-pipe)
  (defproxy xpath-protocol:node-type-p type))

(define-default-method xpath-protocol:node-p ((node stripping-node))
  t)

(define-default-method xpath-protocol:child-pipe ((node stripping-node))
  (stripping-node-children node))

(define-default-method xpath-protocol:parent-node ((node stripping-node))
  (stripping-node-parent node))

(define-default-method xpath-protocol:node-text ((node stripping-node))
  (with-output-to-string (s)
    (write-string-value node s)))

(defmethod write-string-value ((node parent-stripping-node) stream)
  (do-pipe (child (xpath-protocol:child-pipe node))
    (unless (or (xpath-protocol:node-type-p child :comment)
                (xpath-protocol:node-type-p child :processing-instruction))
      (write-string-value child stream))))

(defmethod write-string-value ((node leaf-stripping-node) stream)
  (write-string-value (stripping-node-target node) stream))

(defmethod write-string-value (node stream)
  (write-string (xpath-protocol:node-text node) stream))

(define-default-method xpath-protocol:get-element-by-id
    ((node stripping-node) id)
  (xpath-protocol:get-element-by-id (stripping-node-target node) id))

(define-default-method xpath-protocol:unparsed-entity-uri
    ((node stripping-node) name)
  (xpath-protocol:unparsed-entity-uri (stripping-node-target node) name))


;;;; TEXT NORMALIZER, from cxml-rng

;;; FIXME: cxml should do that

(defun make-text-normalizer (next)
  (make-instance 'text-normalizer :chained-handler next))

(defclass text-normalizer (cxml:sax-proxy)
  ((pending-text-node :initform (make-string-output-stream)
                      :accessor pending-text-node)))

(defmethod sax:characters ((handler text-normalizer) data)
  (write-string data (pending-text-node handler)))

(defun flush-pending (handler)
  (let ((str (get-output-stream-string (pending-text-node handler))))
    (unless (zerop (length str))
      (sax:characters (cxml:proxy-chained-handler handler) str))))

(defmethod sax:start-element :before
    ((handler text-normalizer) uri lname qname attributes)
  (declare (ignore uri lname qname attributes))
  (flush-pending handler))

(defmethod sax:end-element :before
    ((handler text-normalizer) uri lname qname)
  (declare (ignore uri lname qname))
  (flush-pending handler))
