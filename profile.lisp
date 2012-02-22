;;; -*- show-trailing-whitespace: t; indent-tabs-mode: nil -*-

;;; Copyright (c) 2008 David Lichteblau, Ivan Shvedunov.
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

#+sbcl
(declaim (optimize (debug 2)))


;;;; profiling support


;;; zzz Some profiling overhead is incurred even while profiling is disabled,
;;; because we check *profiling-enable-p* at run time, not compilation time.
;;; Reading one extra special variable for each template can't be a huge
;;; problem though.  (Well, and for each serialization function call.)


(defvar *profiling-callers* nil)
(defvar *samples* nil)

(defun clear-counter (counter)
  (setf (profile-counter-calls counter) 0)
  (setf (profile-counter-run counter) 0)
  (setf (profile-counter-real counter) 0))

(defun counter- (a b &rest rest)
  (if rest
      (apply #'counter- (counter- a b) rest)
      (make-profile-counter
       :calls (- (profile-counter-calls a) (profile-counter-calls b))
       :real (- (profile-counter-real a) (profile-counter-real b))
       :run (- (profile-counter-run a) (profile-counter-run b)))))

(defun report-counter (counter label &optional (callsp t))
  (format t "  ~A:~40T~5D run ~5D real~@[ (~D calls)~]~%"
	  label
	  (profile-counter-run counter)
	  (profile-counter-real counter)
	  (and callsp (profile-counter-calls counter))))

(defun enable-profiling ()
  "@return{nil}
   @short{Enables profiling.}

   Resets any existing profile samples and enables profiling for future
   XSLT processing.

   Also enables XPath profiling, see @fun{xpath-sys:enable-profiling}.

   Profiling is not thread safe.

   @see{disable-profiling}
   @see{report}"
  (setf *profiling-enabled-p* t)
  (setf *samples* nil)
  (clear-counter *apply-stylesheet-counter*)
  (clear-counter *parse-stylesheet-counter*)
  (clear-counter *parse-xml-counter*)
  (clear-counter *unparse-xml-counter*)
  (format t "~&XSLT profiling enabled.  (0 samples now recorded)~%~%")
  (xpath-sys:enable-profiling nil))

(defun disable-profiling ()
  "@return{nil}
   @short{Disables profiling.}
   
   Disables profiling for future XSLT processing, but keeps recorded
   profiling samples for @fun{report}.

   Also disables XPath profiling, see @fun{xpath-sys:disable-profiling}.

   @see{enable-profiling}"
  (setf *profiling-enabled-p* nil)
  (format t "~&XSLT profiling disabled.  (~D sample~:P currently recorded)~%"
	  (length *samples*))
  (xpath-sys:disable-profiling))

(defun invoke-template/profile (ctx template param-bindings)
  (let ((run0 (get-internal-run-time))
        (real0 (get-internal-real-time)))
    (unwind-protect
         (let ((*profiling-callers* (cons template *profiling-callers*)))
           (invoke-template ctx template param-bindings))
      (let* ((run1 (get-internal-run-time))
             (real1 (get-internal-real-time))
             (run (- run1 run0))
             (real (- real1 real0)))
        (push (list template *profiling-callers* run real) *samples*)))))

(defun invoke-with-profile-counter (fn counter)
  (let ((run0 (get-internal-run-time))
	(real0 (get-internal-real-time)))
    (unwind-protect
	 (funcall fn)
      (let* ((run1 (get-internal-run-time))
	     (real1 (get-internal-real-time))
	     (run (- run1 run0))
	     (real (- real1 real0)))
	(incf (profile-counter-calls counter))
	(incf (profile-counter-run counter) run)
	(incf (profile-counter-real counter) real)))))

(defstruct (profile-data
	     (:constructor make-profile-data (template))
	     (:conc-name "DATA-"))
  template
  (total-real 0)
  (total-run 0)
  (self-real 0)
  (self-run 0)
  (calls 0))

(defun group-and-sort-samples ()
  (let ((table (make-hash-table)))
    (loop
       for (callee callers run real) in *samples*
       do
	 (let ((data
		(or (gethash callee table)
		    (setf (gethash callee table)
			  (make-profile-data callee)))))
	   (unless (find callee callers)
	     (incf (data-total-run data) run)
	     (incf (data-total-real data) real))
	   (incf (data-self-run data) run)
	   (incf (data-self-real data) real)
	   (incf (data-calls data)))
	 (when callers
	   (let* ((caller (car callers))
		  (data
		   (or (gethash caller table)
		       (setf (gethash caller table)
			     (make-profile-data caller)))))
	     (decf (data-self-run data) run)
	     (decf (data-self-real data) real))))
    (sort (loop
	     for data being each hash-value in table
	     collect data)
	  #'>
	  :key #'data-total-run)))

(defun report-samples (template-times)
  (format t "~&~D Template~:P called:~%~%"
	  (length template-times))
  (format t "   run   real      #   avg.run run   real  template~%")
  (format t "   total total         total   self  self~%~%")
  (let ((base-uris (make-hash-table :test #'equal)))
    (dolist (data template-times)
      (let ((template (data-template data)))
	(format t "~6D ~6D ~6D ~6D ~6D ~6D  "
		(data-total-run data)
		(data-total-real data)
		(data-calls data)
		(floor (data-total-run data) (data-calls data))
		(data-self-run data)
		(data-self-real data))
	(let ((base-uri (template-base-uri template)))
	  (format t "<~D> "
		  (or (gethash base-uri base-uris)
		      (setf (gethash base-uri base-uris)
			    (1+ (hash-table-count base-uris))))))
	(if (template-name template)
	    (format t "name=~S" (template-unparsed-qname template))
	    (format t "match=~S" (xpath::stringify-pattern-expression
				  (template-match-expression template))))
	(when (template-mode-qname template)
	  (format t ", mode=~S" (template-mode-qname template)))
	(format t "~%~%")))
    (format t "~%Index of stylesheets:~%~%")
    (let ((sorted-base-uris
	   (sort (loop
		    for base-uri being each hash-key
		    in base-uris
		    using (hash-value id)
		    collect (cons id base-uri))
		 #'<
		 :key #'car)))
      (loop
	 for (id . base-uri) in sorted-base-uris
	 do (format t " <~D> = ~A~%" id base-uri)))))

(defun report ()
  "@short{Shows profiling output.}

   Shows cumulative run time and real time, number of calls, and average
   run time for each template that was invoked.

   @see{enable-profiling}
   @see{disable-profiling}"
  (format t "~&~D template call~:P recorded~%~%" (length *samples*))
  (format t "1 second = ~D time units~%~%"
	  internal-time-units-per-second)
  (report-counter *apply-stylesheet-counter* "Stylesheet application (total)")
  (report-counter *parse-stylesheet-counter* "  ... XSLT compilation")
  (report-counter *parse-xml-counter* "  ... XML parsing")
  (report-counter *unparse-xml-counter* "  ... Serialization" nil)
  (format t       "  ----------------------------------------------------------------------~%")
  (report-counter (counter- *apply-stylesheet-counter*
			    *parse-stylesheet-counter*
			    *parse-xml-counter*
			    *unparse-xml-counter*)
		  "      Remaining XSLT processing time"
		  nil)
  (terpri)
  (terpri)
  (loop
     for (nil run real) in xpath::*samples*
     count t into calls
     sum run into total-run
     sum real into total-real
     finally
       (report-counter
	(make-profile-counter :calls calls :run total-run :real total-real)
	"Includes XPath processing"))
  (format t "(Valid only if XPath profiling was enabled during XSLT compilation.)")
  (terpri)
  (terpri)
  (report-samples (group-and-sort-samples)))
