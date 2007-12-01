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

(defparameter *tests-directory*
    "/home/david/src/XSLT-testsuite-04/testsuite/TESTS/")

(defclass test-case ()
  ((id :initarg :id :accessor test-id)
   (category :initarg :category :accessor test-category)
   (operation :initarg :operation :accessor test-operation)
   (data-pathname :initarg :data-pathname :accessor test-data-pathname)
   (stylesheet-pathname :initarg :stylesheet-pathname
			:accessor test-stylesheet-pathname)))

(defmethod print-object ((object test-case) stream)
  (print-unreadable-object (object stream :identity nil :type t)
    (format stream "~A ~A/~A"
	    (test-operation object)
	    (test-category object)
	    (test-id object))))


;;;; SIMPLIFY-TESTS

;;; Translate catalog.xml into an actually usable katalog.xml
;;; by running the test cases through xsltproc to see what it thinks
;;; about them.

(defun simplify-tests (&optional (d *tests-directory*))
  (with-open-file (stream (merge-pathnames "katalog.xml" d)
			  :direction :output
			  :if-exists :supersede
			  :element-type '(unsigned-byte 8))
    (cxml:with-xml-output (cxml:make-octet-stream-sink stream)
      (cxml:with-element "simplified-test-suite"
	(klacks:with-open-source
	    (source (klacks:make-tapping-source
		     (cxml:make-source (merge-pathnames "catalog.xml" d))))
	  (let ((*default-pathname-defaults* (merge-pathnames d)))
	    (map-original-tests #'simplify-test source)))))))

(defun map-original-tests (run-test source &key (test (constantly t)))
  (let ((total 0)
	(pass 0)
	major-path)
    (loop
       while (klacks:find-event source :start-element)
       for lname = (klacks:current-lname source)
       do
       (cond
	 ((equal lname "major-path")
	  (klacks:skip source :start-element)
	  (setf major-path
		(namestring
		 (merge-pathnames (klacks:consume-characters source)))))
	 ((equal lname "test-case")
	  (let* ((<test-case>
		  (stp:document-element
		   (klacks:serialize-element source (stp:make-builder))))
		 (test-case (parse-original-test major-path <test-case>)))
	    (when (funcall test test-case)
	      (incf total)
	      (when (funcall run-test test-case)
		(incf pass)))))
	 (t
	  (klacks:skip source :start-element))))
    (format t "~&Passed ~D/~D tests.~%" pass total)))

(defun parse-original-test (major-path <test-case>)
  (let* ((file-path
	  (stp:string-value
	   (stp:find-recursively-if (stp:of-name "file-path") <test-case>)))
	 (base (concatenate 'string major-path "/" file-path))
	 (scenario
	  (stp:find-recursively-if (stp:of-name "scenario") <test-case>))
	 data
	 stylesheet
	 supplemental-stylesheet
	 supplemental-data)
    (dolist
	(input (stp:filter-recursively (stp:of-name "input-file") <test-case>))
      (let ((role (stp:attribute-value input "role"))
	    (path (concatenate 'string base "/" (stp:string-value input))))
	(cond
	  ((equal role "principal-data")
	   (setf data path))
	  ((equal role "principal-stylesheet")
	   (setf stylesheet path))
	  ((equal role "supplemental-stylesheet")
	   (setf supplemental-stylesheet path))
	  ((equal role "supplemental-data")
	   (setf supplemental-data path))
	  (t
	   (error "unrecognized role: ~A" role)))))
    (make-instance 'test-case
		   :id (stp:attribute-value <test-case> "id")
		   :category (stp:attribute-value <test-case> "category")
		   :operation (stp:attribute-value scenario "operation")
		   :data-pathname data
		   :stylesheet-pathname stylesheet)))

(defun write-simplified-test (test-case operation)
  (cxml:with-element "test-case"
    (cxml:attribute "id" (test-id test-case))
    (cxml:attribute "category" (test-category test-case))
    (cxml:attribute "data" (namestring (test-data-pathname test-case)))
    (cxml:attribute "stylesheet"
		    (namestring (test-stylesheet-pathname test-case)))
    (cxml:attribute "operation" operation)))

(defun test-output-pathname (test type)
  (make-pathname :name (test-id test)
		 :type type
		 :defaults (test-data-pathname test)))

(defun simplify-test (test-case)
  (flet ((report (status &optional (fmt "") &rest args)
	   (format t "~&~A ~A [~A]~?~%"
		   status
		   (test-id test-case)
		   (test-category test-case)
		   fmt
		   args)))
    (let* ((data (test-data-pathname test-case))
	   (stylesheet (test-stylesheet-pathname test-case))
	   (out (test-output-pathname test-case "xsltproc")))
      (if (equal (test-operation test-case) "standard")
	  (handler-case
	      (progn
		(xsltproc stylesheet data out)
		(report "PASS")
		(write-simplified-test test-case "standard")
		t)
	    (error (c)
	      (report "FAIL" ": ~A" c)
	      (write-simplified-test test-case "execution-error")
	      nil))
	  (handler-case
	      (progn
		(xsltproc stylesheet data "/dev/null")
		(report "FAIL" ": expected error not signalled")
		;; let's ignore unexpected successes for now
		nil)
	    (error (c)
	      (report "PASS" ": expected error ~A" c)
	      (write-simplified-test test-case "execution-error")
	      t))))))

(defun xsltproc (stylesheet input output)
  (flet ((full-namestring (x)
	   (namestring (merge-pathnames x))))
    (let* ((asdf::*verbose-out* (make-string-output-stream))
	   (code (asdf:run-shell-command
		  "cd ~S && xsltproc ~S ~S >~S"
		  (full-namestring "")
		  (full-namestring stylesheet)
		  (full-namestring input)
		  (full-namestring output))))
      (unless (zerop code)
	(error "running xsltproc failed with code ~A [~%~A~%]"
	       code
	       (get-output-stream-string asdf::*verbose-out*))))))


;;;; RUN-TESTS and DRIBBLE-TESTS

;;; Process katalog.xml

;; temporary configuration until we support enough XSLT that it's worth
;; running all tests:
(defparameter *default-categories*
  '("XSLT-Data-Model" "XPath-Expression"))

(defun dribble-tests (&optional (category *default-categories*)
		      (d *tests-directory*))
  (with-open-file (dribble
		   (merge-pathnames "TEST"
				    (slot-value (asdf:find-system :xuriella)
						'asdf::relative-pathname))
		   :direction :output
		   :if-exists :supersede
		   :external-format :utf8)
    (let* ((dribble (make-broadcast-stream dribble *standard-output*))
	   (*standard-output* dribble)
	   (*trace-output* dribble)
	   (*error-output* dribble)
	   (*terminal-io* (make-two-way-stream *standard-input* dribble)))
      (run-tests category d))))

(defun run-tests (&optional (categories *default-categories*)
		  (d *tests-directory*))
  (unless (listp categories)
    (setf categories (list categories)))
  (klacks:with-open-source
      (source (klacks:make-tapping-source
	       (cxml:make-source (merge-pathnames "katalog.xml" d))))
    (let ((*default-pathname-defaults* (merge-pathnames d)))
      (map-tests #'run-test
		 source
		 :test (lambda (test)
			 (or (null categories)
			     (find (test-category test)
				   categories
				   :test #'equal)))))))

(defun map-tests (run-test source &key (test (constantly t)))
  (let ((total 0)
	(pass 0))
    (loop
       while (klacks:find-event source :start-element)
       for lname = (klacks:current-lname source)
       do
       (cond
	 ((equal lname "test-case")
	  (let* ((<test-case>
		  (stp:document-element
		   (klacks:serialize-element source (stp:make-builder))))
		 (test-case (parse-test <test-case>)))
	    (when (funcall test test-case)
	      (incf total)
	      (when (funcall run-test test-case)
		(incf pass)))))
	 (t
	  (klacks:skip source :start-element))))
    (format t "~&Passed ~D/~D tests.~%" pass total)))

(defun parse-test (<test-case>)
  (make-instance 'test-case
		 :id (stp:attribute-value <test-case> "id")
		 :category (stp:attribute-value <test-case> "category")
		 :operation (stp:attribute-value <test-case> "operation")
		 :data-pathname (stp:attribute-value <test-case> "data")
		 :stylesheet-pathname (stp:attribute-value
				       <test-case> "stylesheet")))

(defun output-equal-p (p q)
  (let ((r (cxml:parse p (stp:make-builder)))
	(s (cxml:parse q (stp:make-builder))))
    (node= r s)))

(defun run-test (test)
  (let ((expected (test-output-pathname test "xsltproc"))
	(actual (test-output-pathname test "xuriella")))
    (flet ((doit ()
	     (with-open-file (s actual
				:if-exists :rename-and-delete
				:direction :output
				:element-type '(unsigned-byte 8))
	       (apply-stylesheet (pathname (test-stylesheet-pathname test))
				 (pathname (test-data-pathname test))
				 s)))
	   (report (status &optional (fmt "") &rest args)
	     (format t "~&~A ~A [~A]~?~%"
		     status
		     (test-id test)
		     (test-category test)
		     fmt
		     args)))
      (cond
	((equal (test-operation test) "standard")
	 (handler-case
	     (progn
	       (doit)
	       (handler-case
		   (cond
		     ((output-equal-p expected actual)
		      (report "PASS")
		      t)
		     (t
		      (report "FAIL"
			      ": expected output ~A but found ~A"
			      expected
			      actual)
		      nil))
		 ((or error parse-number::invalid-number) (c)
		   (report "FAIL" ": comparison failed: ~A" c))))
	   ((or error parse-number::invalid-number) (c)
	     (report "FAIL" ": ~A" c)
	     nil)))
	(t
	 (handler-case
	     (doit)
	   ((or error parse-number::invalid-number) (c)
	     (report "PASS" ": expected error ~A" c)
	     t)
	   (:no-error (result)
	     (report "FAIL" ": expected error not signalled: " result)
	     nil)))))))

(defun run-xpath-tests ()
  (run-tests '("XPath-Expression" "XSLT-Data-Model")))


;;;; from cxml-stp-test

(defun assert-node= (a b)
  (unless (node= a b)
    (error "assertion failed: ~S and ~S are not NODE=" a b)))

(defun child-count (node)
  (stp:count-children-if (constantly t) node))

(defun named-node-= (a b)
  (and (equal (stp:namespace-uri a) (stp:namespace-uri b))
       (equal (stp:namespace-prefix a) (stp:namespace-prefix b))
       (equal (stp:local-name a) (stp:local-name b))))

(defun parent-node-= (e f)
  (and (eql (child-count e)
	    (child-count f))
       (every #'node= (stp:list-children e) (stp:list-children f))))

(defmethod node= ((e stp:element) (f stp:element))
  (and (named-node-= e f)
       (parent-node-= e f)
       (null
	(set-exclusive-or (stp:list-attributes e) (stp:list-attributes f)
			  :test #'node=))
       (flet ((collect-namespaces (elt)
		(let ((result ()))
		  (stp:map-extra-namespaces
		   (lambda (k v) (push (cons k v) result))
		   elt)
		  result)))
	 (null
	  (set-exclusive-or (collect-namespaces e) (collect-namespaces f)
			    :test #'equal)))))

(defmethod node= ((a stp:node) (b stp:node))
  nil)

(defmethod node= ((e stp:document) (f stp:document))
  (parent-node-= e f))

(defmethod node= ((a stp:attribute) (b stp:attribute))
  (and (named-node-= a b)
       (equal (stp:value a) (stp:value b))))

(defmethod node= ((a stp:comment) (b stp:comment))
  (equal (stp:data a) (stp:data b)))

(defmethod node= ((a stp:text) (b stp:text))
  (equal (stp:data a) (stp:data b)))

(defmethod node= ((a stp:processing-instruction)
		  (b stp:processing-instruction))
  (and (equal (stp:data a) (stp:data b))
       (equal (stp:target a) (stp:target b))))

(defmethod node= ((a stp:document-type) (b stp:document-type))
  (and (equal (stp:root-element-name a) (stp:root-element-name b))
       (equal (stp:public-id a) (stp:public-id b))
       (equal (stp:system-id a) (stp:system-id b))
       (equal (stp:internal-subset a) (stp:internal-subset b))))
