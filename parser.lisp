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


(defun parse-body (node &optional (start 0))
  (let ((n (stp:count-children #'identity node)))
    (labels ((recurse (i)
	       (when (< i n)
		 (let ((child (stp:nth-child i node)))
		   (if (namep child "variable")
		       (stp:with-attributes (name select) child
			 `((let ((,name ,(or select
					     `(progn ,@(parse-body child)))))
			     ,@(recurse (1+ i))))))
		   (cons (parse-instruction child)
			 (recurse (1+ i)))))))
      (recurse start)))
  (stp:map-children 'list #'parse-instruction node))

(defun parse-instruction (node)
  (typecase node
    (stp:element
     (if (equal (stp:namespace-uri node) *xsl*)
	 (parse-instruction/xsl-element
	  (find-symbol (stp:local-name node) :xuriella)
	  node)
	 (parse-instruction/literal-element node)))
    (stp:text
     `(xsl:text ,(stp:data node)))))

(defun parse-instruction/literal-element (node)
  `(xsl:literal-element
       (,(stp:local-name node) ,(stp:namespace-uri node))
     ,@(stp:map-attributes 'list
			   (lambda (a)
			     `(xsl:literal-attribute
				  (,(stp:local-name a)
				    ,(stp:namespace-uri a))
				,(stp:value a)))
			   node)
     ,@(stp:map-children 'list #'parse-instruction node)))

(defmacro define-instruction-parser (name (node-var) &body body)
  `(defmethod parse-instruction/xsl-element
       ((.name. (eql ',name)) ,node-var)
     (declare (ignore .name.))
     ,@body))

(define-instruction-parser |apply-templates| (node)
  (stp:with-attributes (select mode) node
    `(xsl:apply-templates
      (:select ,select :mode ,mode))))

(define-instruction-parser |if| (node)
  (stp:with-attributes (test) node
    `(when ,test
       ,@(parse-body node))))

(define-instruction-parser |choose| (node)
  `(cond
     ,@(stp:map-children 'list
			 (lambda (<when>)
			   (stp:with-attributes (test) <when>
			     `(,test
			       ,@(parse-body <when>))))
			 node)))

(define-instruction-parser |element| (node)
  (stp:with-attributes (name namespace use-attribute-sets) node
    `(xsl:element (,name :namespace namespace
			 :use-attribute-sets use-attribute-sets)
       ,@(parse-body node))))

(define-instruction-parser |attribute| (node)
  (stp:with-attributes (name namespace) node
    `(xsl:attribute (,name :namespace namespace)
       ,@(parse-body node))))

(define-instruction-parser |text| (node)
  `(xsl:text ,@(stp:string-value node)))

(define-instruction-parser |comment| (node)
  `(xsl:comment ,@(stp:string-value node)))

(define-instruction-parser |processing-instruction| (node)
  (stp:with-attributes (name) node
    `(xsl:processing-instruction ,name
       ,@(parse-body node))))

(define-instruction-parser |value-of| (node)
  (stp:with-attributes (select disable-output-escaping) node
    (if disable-output-escaping
	`(xsl:unescaped-value-of ,select)
	`(xsl:value-of ,select))))

(define-instruction-parser |for-each| (node)
  (stp:with-attributes (select) node
    (multiple-value-bind (decls body-position)
	(loop
	   for i from 0
	   for child in (stp:list-children node)
	   while (namep node "sort")
	   collect (parse-sort node) into decls
	   finally (return (values decls i)))
      `(xsl:for-each ,select
	 (declare ,@decls)
	 ,@(parse-body node body-position)))))

(defun parse-sort (node)
  (stp:with-attributes (select lang data-type order case-order) node
    `(sort :select ,select
	   :lang ,lang
	   :data-type ,data-type
	   :order ,order
	   :case-order ,case-order)))

(define-instruction-parser |message| (node)
  `(xsl:message ,@(parse-body node)))

(define-instruction-parser |terminate| (node)
  `(xsl:terminate ,@(parse-body node)))