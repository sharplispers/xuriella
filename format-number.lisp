;;; -*- show-trailing-whitespace: t; indent-tabs: nil -*-

;;; Copyright (c) 2008 David Lichteblau. All rights reserved.

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

(defstruct (decimal-format (:conc-name "DF/"))
  ;; picture string and output syntax:
  (decimal-separator #\.)		;active
  (grouping-separator #\,)		;active
  (zero-digit #\0)			;active
  (percent #\%)
  (per-mille (code-char 2030))

  ;; picture string syntax only
  (digit #\#)				;active
  (pattern-separator #\;)		;active

  ;; output syntax only:
  (infinity "Infinity")
  (nan "NaN")
  (minus-sign #\-))

(defstruct (picture (:conc-name "PIC/"))
  percentp
  per-mille-p
  prefix
  suffix
  integer-part-grouping-positions
  minimum-integer-part-size
  fractional-part-grouping-positions
  minimum-fractional-part-size
  maximum-fractional-part-size)

(defun df/active-characters (df)
  (format nil "~C~C~C~C~C"
	  (df/decimal-separator df)
	  (df/grouping-separator df)
	  (df/zero-digit df)
	  (df/digit df)
	  (df/pattern-separator df)))

(defun df/digits (df)
  (let ((result (make-array 10))
	(start (char-code (df/zero-digit df))))
    (loop
       for i from 0 below 10
       do
	 (setf (elt result i) (code-char (+ start i))))
    result))

(defun find-decimal-format (format-name)
  (declare (ignore format-name))
  ;; fixme
  (make-decimal-format))

(xpath::define-xpath-function/eager
    :format-number
    (value picture &optional format-name)
  (let ((df (find-decimal-format format-name)))
    (multiple-value-bind (pos neg)
	(parse-picture (xpath:string-value picture) df)
      (format-number (xpath:number-value value)
		     pos
		     neg
		     df))))

(defun test-format-number (value picture)
  (let ((df (make-decimal-format)))
    (multiple-value-bind (pos neg)
	(parse-picture picture df)
      (format-number value pos neg df))))

(defun parse-picture (picture df)
  (destructuring-bind (&optional positive negative &rest erroneous)
      (split-sequence:split-sequence
       (df/pattern-separator df)
       picture)
    (unless (and positive (not erroneous))
      (error "invalid pattern separators"))
    (unless negative
      (setf negative (concatenate 'string
				  (string (df/minus-sign df))
				  positive)))
    (values (parse-sub-picture positive df)
	    (parse-sub-picture negative df))))

(defmacro df/case (df form &rest clauses)
  `(let ((.form ,form)
	 (.df ,df))
     (cond
       ,@(loop
	    for (accessor . body) in clauses
	    collect `((eql (,accessor .df) .form) ,@body)))))

(defun parse-integer-picture (picture df start end)
  (let ((integer-part-grouping-positions '())
	(minimum-integer-part-size 0))
    (loop
       for i from start below end
       for c = (elt picture i)
       until (eql c (df/decimal-separator df))
       do
	 (df/case df c
	   (df/grouping-separator
	    (push 0 integer-part-grouping-positions))
	   (df/digit
	    (when integer-part-grouping-positions
	      (incf (car integer-part-grouping-positions))))
	   (df/zero-digit
	    (when integer-part-grouping-positions
	      (incf (car integer-part-grouping-positions)))
	    (incf minimum-integer-part-size)))
       finally
	 (return (values i
			 (loop
			    for pos in integer-part-grouping-positions
			    for accum = pos then (+ accum pos)
			    collect accum)
			 minimum-integer-part-size)))))

(defun parse-fractional-picture (picture df start end)
  (let ((fractional-part-grouping-positions '())
	(minimum-fractional-part-size 0)
	(maximum-fractional-part-size 0)
	(current-grouping 0))
    (loop
       for i from start below end
       for c = (elt picture i)
       do
	 (df/case df c
	   (df/grouping-separator
	    (push current-grouping fractional-part-grouping-positions))
	   (df/digit
	    (incf current-grouping)
	    (incf maximum-fractional-part-size))
	   (df/zero-digit
	    (incf current-grouping)
	    (incf minimum-fractional-part-size)
	    (incf maximum-fractional-part-size))
	   (df/decimal-separator))
       finally
	 (return (values (nreverse fractional-part-grouping-positions)
			 minimum-fractional-part-size
			 maximum-fractional-part-size)))))

(defun parse-sub-picture (picture df)
  (let ((active (df/active-characters df)))
    (flet ((activep (x) (find x active)))
      (let ((start (position-if #'activep picture))
	    (last (position-if #'activep picture :from-end t)))
	(unless start
	  (error "no digit-sign or zero-digit sign found"))
	(let* ((end (1+ last))
	       (result (make-picture
                         :percentp (find (df/percent df) picture)
			 :per-mille-p (find (df/per-mille df) picture)
			 :prefix (subseq picture 0 start)
			 :suffix (subseq picture end)))) 
	  (setf (values start
			(pic/integer-part-grouping-positions result)
			(pic/minimum-integer-part-size result))
		(parse-integer-picture picture df start end))
	  (setf (values (pic/fractional-part-grouping-positions result)
			(pic/minimum-fractional-part-size result)
			(pic/maximum-fractional-part-size result))
		(parse-fractional-picture picture df start end))
	  result)))))

(defun nanp (value)
  ;; fixme
  (eq value :nan))

(defun infinityp (value)
  (eq value :infinity))

(defun format-number (value positive-picture negative-picture df)
  (if (nanp value)
      (df/nan df)
      (let ((picture (if (minusp value) negative-picture positive-picture)))
	(if (infinityp value)
	    (concatenate 'string
			 (pic/prefix picture)
			 (df/infinity df)
			 (pic/suffix picture))
	    (format-ordinary-number value picture df)))))

(defun format-number-~f (number picture df)
  (let* ((str (format nil "~,vF"
		      (pic/maximum-fractional-part-size picture)
		      number))
	 (str (string-trim (string (df/zero-digit df)) str)) ;for 0.0
	 (digits (df/digits df)))
    (map 'string
	 (lambda (x)
	   (if (eql x #\.)
	       (df/decimal-separator df)
	       (elt digits (- (char-code x) #.(char-code #\0)))))
	 str)))

(defun make-grouping-test (positions)
  (if (and positions
	   (let ((first (car positions)))
	     (loop
		for expected = first then (+ expected first)
		for pos in positions
		always (eql pos expected))))
      (let ((first (car positions)))
	(lambda (x)
	  (and (plusp x) (zerop (mod x first)))))
      (lambda (x)
	(and (plusp x) (find x positions)))))

(defun format-ordinary-number (value picture df)
  (let* ((adjusted-number
	  (cond
	    ((pic/percentp picture)
	     (* value 100))
	    ((pic/per-mille-p picture)
	     (* value 1000))
	    (t
	     value)))
	 (str (format-number-~f (abs adjusted-number) picture df))
	 (left (position (df/decimal-separator df) str))
	 (right (1- (- (length str) left)))
	 (wanted-left (max left (pic/minimum-integer-part-size picture)))
	 (wanted-right (max right (pic/minimum-fractional-part-size picture)))
	 (zero (df/zero-digit df))
	 (left-test (make-grouping-test
		     (pic/integer-part-grouping-positions picture)))
	 (right-test (make-grouping-test
		      (pic/fractional-part-grouping-positions picture))))
    (with-output-to-string (s)
      (write-string (pic/prefix picture) s)
      (loop
	 for i from (1- wanted-left) downto 0
	 for index from 0
	 do
	   (if (< i left)
	       (write-char (elt str index) s)
	       (write-char zero s))
	   (when (funcall left-test i)
	     (write-char (df/grouping-separator df) s)))
      (when (plusp wanted-right)
	(write-char (df/decimal-separator df) s)
	(loop
	   for i from 0 below wanted-right
	   for index from (+ left 1)
	   do
	     (when (funcall right-test i)
	       (write-char (df/grouping-separator df) s))
	     (if (< i right)
		 (write-char (elt str index) s)
		 (write-char zero s))))
      (write-string (pic/suffix picture) s))))
