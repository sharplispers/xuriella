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
        (or (sink-encoding handler) "UTF-8")))

(defmethod sax:start-document ((handler combi-sink))
  nil)

(defmethod sax:start-dtd ((handler combi-sink) name pubid sysid)
  (when (or pubid sysid)
    (hax:start-document (sink-hax-target handler) name pubid sysid)))

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




;;; Handler for the default output method.
;;;
;;; Waits for the document element, then decides between combi-sink and
;;; xml sink.
;;;
;;; Also figures out the root element name for the doctype.

(defclass auto-detect-sink (cxml:broadcast-handler)
  ((switchedp :initform nil :accessor sink-switched-p)
   (detected-method :initarg :detected-method :accessor sink-detected-method)
   (sysid :initform nil :accessor sink-sysid)
   (pubid :initform nil :accessor sink-pubid)
   (buffered-events :initform '() :accessor sink-buffered-events)))

(defun make-auto-detect-sink (combi-sink fixed-method)
  (make-instance 'auto-detect-sink
                 :handlers (list combi-sink)
                 :detected-method fixed-method))

(defmethod sax:start-document ((handler auto-detect-sink))
  nil)

(defmethod sax:start-dtd ((handler auto-detect-sink) name pubid sysid)
  (setf (sink-sysid handler) sysid)
  (setf (sink-pubid handler) pubid))

(defmethod sax:start-element
    :before
    ((handler auto-detect-sink) uri lname qname attrs)
  (unless (sink-switched-p handler)
    (if (ecase (sink-detected-method handler)
          (:html t)
          (:xml nil)
          ((nil) (and (equal uri "") (string-equal lname "html"))))
        (switch-to-html-output handler qname)
        (switch-to-xml-output handler qname))))

(defmethod sax:end-document :before ((handler auto-detect-sink))
  (unless (sink-switched-p handler)
    (if (eq (sink-detected-method handler) :html)
        (switch-to-html-output handler "root")
        (switch-to-xml-output handler "root"))))

(defmethod sax:characters ((handler auto-detect-sink) data)
  (cond
    ((sink-switched-p handler)
     (call-next-method))
    (t
     (unless (or (whitespacep data) (sink-detected-method handler))
       (setf (sink-detected-method handler) :xml))
     (push (list 'sax:characters data) (sink-buffered-events handler)))))

(defmethod sax:processing-instruction
    ((handler auto-detect-sink) target data)
  (cond
    ((sink-switched-p handler)
     (call-next-method))
    (t
     (push (list 'sax:processing-instruction target data)
           (sink-buffered-events handler)))))

(defmethod sax:unescaped ((handler auto-detect-sink) data)
  (cond
    ((sink-switched-p handler)
     (call-next-method))
    (t
     (push (list 'sax:unescaped data) (sink-buffered-events handler)))))

(defmethod sax:comment ((handler auto-detect-sink) data)
  (cond
    ((sink-switched-p handler)
     (call-next-method))
    (t
     (push (list 'sax:comment data) (sink-buffered-events handler)))))

(define-condition |hey test suite, this is an HTML document| ()
  ())

(defun switch-to-html-output (handler qname)
  (signal '|hey test suite, this is an HTML document|)
  (setf (sink-switched-p handler) t)
  (when (or (sink-sysid handler) (sink-pubid handler))
    (hax:start-document (car (cxml:broadcast-handler-handlers handler))
                        qname
                        (sink-pubid handler)
                        (sink-sysid handler)))
  (replay-buffered-events handler))

(defun switch-to-xml-output (handler qname)
  (setf (sink-switched-p handler) t)
  (let ((target
         (sink-sax-target (car (cxml:broadcast-handler-handlers handler)))))
    (setf (cxml:broadcast-handler-handlers handler) (list target))
    (sax:start-document target)
    (when (sink-sysid handler)
      (sax:start-dtd target qname (sink-pubid handler) (sink-sysid handler))
      (sax:end-dtd target)))
  (replay-buffered-events handler))

(defun replay-buffered-events (handler)
  (loop
     for (event . args) in (nreverse (sink-buffered-events handler))
     do (apply event handler args)))
