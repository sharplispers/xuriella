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

(in-package :xuriella)


;;; Handler for the HTML output method.
;;;
;;; Dispatches requests to either an HTML sink or an XML sink, depending
;;; on the namespace of the event.
;;;
;;; Inserts the http-equiv meta tag.

(defclass combi-sink (sax:content-handler)
  ((hax-target :initarg :hax-target :accessor sink-hax-target)
   (sax-target :initarg :sax-target :accessor sink-sax-target)
   (encoding :initarg :encoding :accessor sink-encoding)))

(defmethod initialize-instance :after ((handler combi-sink) &key)
  (setf (sink-encoding handler)
        (or (sink-encoding handler) "utf-8")))

(defmethod sax:start-document ((handler combi-sink))
  nil)

(defmethod sax:start-dtd ((handler combi-sink) name pubid systemid)
  (hax:start-document (sink-hax-target handler) name pubid systemid))

(defun maybe-close-tag (combi-sink)
  (cxml::maybe-close-tag (sink-sax-target combi-sink)))

(defmethod sax:start-element ((handler combi-sink) uri lname qname attrs)
  (with-slots (hax-target sax-target encoding) handler
    (maybe-close-tag handler)
    (cond
      ((equal uri "")
       (sax:start-element hax-target *html* lname qname attrs)
       (when (and encoding (equalp lname "head"))
         (let* ((content (format nil "text/html; charset=~A" encoding))
                (attrs
                 (list (hax:make-attribute "http-equiv" "Content-Type")
                       (hax:make-attribute "content" content))))
           (sax:start-element hax-target *html* "meta" "meta" attrs)
           (sax:end-element hax-target *html* "meta" "meta"))))
      (t
       (sax:start-element sax-target uri lname qname attrs)))))

(defmethod sax:end-element ((handler combi-sink) uri lname qname)
  (with-slots (hax-target sax-target) handler
    (maybe-close-tag handler)
    (if (equal uri "")
        (sax:end-element hax-target *html* lname qname)
        (sax:end-element sax-target uri lname qname))))

(defmethod sax:end-document ((handler combi-sink))
  (hax:end-document (sink-hax-target handler)))

(defmethod sax:processing-instruction ((handler combi-sink) target data)
  (maybe-close-tag handler)
  (sax:processing-instruction (sink-hax-target handler) target data))

(defmethod sax:characters ((handler combi-sink) data)
  (maybe-close-tag handler)
  (sax:characters (sink-hax-target handler) data))

(defmethod sax:unescaped ((handler combi-sink) data)
  (maybe-close-tag handler)
  (sax:unescaped (sink-hax-target handler) data))

(defmethod sax:comment ((handler combi-sink) data)
  (maybe-close-tag handler)
  (sax:comment (sink-hax-target handler) data))
