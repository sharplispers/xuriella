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

(define-instruction xsl:number (args env)
  (destructuring-bind (&key level count from value format lang letter-value
                            grouping-separator grouping-size)
      args
    (let ((count (and count (compile-pattern count env)))
          (from  (and from (compile-pattern from env)))
          (value (and value (compile-xpath value env)))
          (format       (compile-avt (or format "1") env))
          (lang         (compile-avt (or lang "") env))
          (letter-value (compile-avt (or letter-value "foo") env))
          (grouping-separator
           (and grouping-separator (compile-avt grouping-separator env)))
          (grouping-size (and grouping-size (compile-avt grouping-size env))))
      (lambda (ctx)
        (let ((value              (when value
                                    (round (xpath:number-value
                                            (funcall value ctx)))))
              (format             (funcall format ctx))
              (lang               (funcall lang ctx))
              (letter-value       (funcall letter-value ctx))
              (grouping-separator (when grouping-separator
                                    (funcall grouping-separator ctx)))
              (grouping-size      (when grouping-size
                                    (funcall grouping-size ctx))))
          (write-text
           (format-number-list
            (if value
		(list value)
                (compute-number-list (or level "single")
                                     (xpath::context-node ctx)
                                     count
                                     from))
            format
            lang
            letter-value
            grouping-separator
            grouping-size)))))))

(defun compile-pattern (str env)
  (compile-xpath
   `(xpath:xpath
     (:union
      ,@(mapcar #'naive-pattern-expression (parse-pattern str))))
   env))

(defun pattern-thunk-matches-p (pattern-thunk node)
  (find node
        (xpath:all-nodes (funcall pattern-thunk (xpath:make-context node)))))

(defun ancestors-using-count-and-from (node count from)
  (let ((ancestors
         (xpath::force
          (funcall (xpath::axis-function :ancestor-or-self) node))))
    (remove-if-not (lambda (ancestor)
                     (pattern-thunk-matches-p count ancestor))
                   (if from
                       (loop
                          for a in ancestors
                          when (pattern-thunk-matches-p from a)
                          do (return result)
                          collect a into result
                          finally (return nil))
                       ancestors))))

(defun node-position-among-siblings (node count)
  (1+
   (count-if (lambda (sibling)
               (pattern-thunk-matches-p count sibling))
             (xpath::force
              (funcall (xpath::axis-function :preceding-sibling) node)))))

(defun compute-number-list (level node count from)
  (unless count
    (setf count
          (let ((uri (xpath-protocol:namespace-uri node))
		(lname (xpath-protocol:local-name node)))
            (lambda (ctx)
              (let ((node (xpath:context-node ctx)))
                (xpath-sys:make-node-set
                 (if (and (xpath-protocol:node-type-p node :element)
			  (equal (xpath-protocol:namespace-uri node) uri)
			  (equal (xpath-protocol:local-name node) lname))
                     (list node)
                     nil)))))))
  (cond
    ((equal level "single")
     (let ((ancestor (car (ancestors-using-count-and-from node count from))))
       (if ancestor
           (list (node-position-among-siblings ancestor count))
           nil)))
    ((equal level "multiple")
     (mapcar (lambda (ancestor)
               (node-position-among-siblings ancestor count))
             (reverse
              (ancestors-using-count-and-from node count from))))
    ((equal level "any")
     (destructuring-bind (root)
         (xpath::force (funcall (xpath::axis-function :root) node))
       (let ((nodes (xpath::force
		     (xpath::append-pipes
		      (xpath::subpipe-before
		       node
		       (funcall (xpath::axis-function :descendant-or-self) root))
		      (list node)))))
	 (when from
           (loop
              for (current . rest) on nodes
	      when (pattern-thunk-matches-p from current)
	      do
		(setf nodes rest)))
         (list
          (loop
             for n in nodes
             count (pattern-thunk-matches-p count n))))))
    (t
     (xslt-error "invalid number level: ~A" level))))

(xpath::deflexer format-lexer
  ("([a-zA-Z0-9]+)" (x) (values :format x))
  ("([^a-zA-Z0-9]+)" (x) (values :text x)))

(defun format-number-token (str n)
  (cond
    ((or (equal str "a") (equal str "A"))
     (let ((start (if (equal str "a") 96 64)))
       (if (zerop n)
	   (code-char (1+ start))
	   (nreverse
	    (with-output-to-string (r)
	      (loop
		 for m = n then rest
		 while (plusp m)
		 for (rest digit) = (multiple-value-list
				     (truncate m 26))
		 do
		 (write-char (code-char (+ start digit)) r)))))))
    ((equal str "i")
     (format nil "~(~@R~)" n))
    ((equal str "I")
     (format nil "~@R" n))
    (t
     (unless (cl-ppcre:all-matches "^0*1$" str)
       ;; unsupported format
       (setf str "1"))
     (format nil "~v,'0D" (length str) n))))

(defun group-numbers (str separator size)
  (loop
     for c across str
     for i from (1- (length str)) downto 0
     do
       (write-char c)
       (when (and (zerop (mod i size)) (plusp i))
	 (write-string separator))))

;;; fixme: unicode support
(defun format-number-list
    (list format lang letter-value grouping-separator grouping-size)
  (declare (ignore lang letter-value))
  (with-output-to-string (*standard-output*)
    (let ((lexer (format-lexer format))
	  (seen-text-p t)
	  (last-token nil))
      (loop
	 (multiple-value-bind (type str) (funcall lexer)
	   (unless type
	     (if list
		 (setf type :format
		       str last-token)
		 (return)))
	   (ecase type
	     (:text
	      (write-string str)
	      (setf seen-text-p t))
	     (:format
	      (unless seen-text-p
		(write-char #\.))
	      (setf seen-text-p nil)
	      (setf last-token str)
	      (let* ((n (pop list))
		     (formatted (format-number-token str n)))
		(write-string (if (and grouping-separator
				       grouping-size)
				  (group-numbers formatted
						 grouping-separator
						 grouping-size)
				  formatted))))))))))
