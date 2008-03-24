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

(defparameter *tests-directory*
  "/home/david/src/XSLT-testsuite-04/testsuite/TESTS/")

(defclass test-case ()
  ((id :initarg :id :accessor test-id)
   (category :initarg :category :accessor test-category)
   (operation :initarg :operation :accessor test-operation)
   (data-pathname :initarg :data-pathname :accessor test-data-pathname)
   (stylesheet-pathname :initarg :stylesheet-pathname
                        :accessor test-stylesheet-pathname)
   (data-pathname-2 :initarg :data-pathname-2 :accessor test-data-pathname-2)
   (stylesheet-pathname-2 :initarg :stylesheet-pathname-2
                          :accessor test-stylesheet-pathname-2)
   (output-pathname :initarg :output-pathname
                    :accessor test-official-output-pathname)
   (output-compare :initarg :output-compare
                   :accessor test-output-compare)))

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
         (out-base (concatenate 'string major-path "/REF_OUT/" file-path))
         (scenario
          (stp:find-recursively-if (stp:of-name "scenario") <test-case>))
         data
         stylesheet
         supplemental-stylesheet
         supplemental-data
         output
         compare)
    (dolist (<input> (stp:filter-recursively (stp:of-name "input-file")
                                             <test-case>))
      (let ((role (stp:attribute-value <input> "role"))
            (path (concatenate 'string base "/" (stp:string-value <input>))))
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
    (dolist (<output> (stp:filter-recursively (stp:of-name "output-file")
                                            <test-case>))
      (let ((role (stp:attribute-value <output> "role"))
            (path (concatenate 'string out-base
                               "/"
                               (stp:string-value <output>))))
        (cond
          ((equal role "principal")
           (setf output path)
           (setf compare (stp:attribute-value <output> "compare")))
          (t
           (error "unrecognized role: ~A" role)))))
    (make-instance 'test-case
                   :id (stp:attribute-value <test-case> "id")
                   :category (stp:attribute-value <test-case> "category")
                   :operation (stp:attribute-value scenario "operation")
                   :data-pathname data
                   :stylesheet-pathname stylesheet
                   :stylesheet-pathname-2 supplemental-stylesheet
                   :data-pathname-2 supplemental-data
                   :output-pathname output
                   :output-compare compare)))

(defun write-simplified-test (test-case operation)
  (cxml:with-element "test-case"
    (cxml:attribute "id" (test-id test-case))
    (cxml:attribute "category" (test-category test-case))
    (flet ((p (l p)
             (cxml:attribute l (and p (namestring p)))))
      (p "data" (test-data-pathname test-case))
      (p "stylesheet" (noindent-stylesheet-pathname test-case))
      (p "data-2" (test-data-pathname-2 test-case))
      (p "stylesheet-2" (test-stylesheet-pathname-2 test-case))
      (p "output" (test-official-output-pathname test-case))
      (p "compare" (test-output-compare test-case)))
    (cxml:attribute "operation" operation)))

(defun test-output-pathname (test type)
  (make-pathname :name (test-id test)
                 :type type
                 :defaults (test-data-pathname test)))

(defun sanitize-stylesheet (in out)
  (if (probe-file in)
      (handler-case
          (let ((d (cxml:parse (pathname in) (stp:make-builder))))
            (xpath:with-namespaces ((nil #.*xsl*))
              (xpath:do-node-set (output (xpath:evaluate "//output" d))
                (let ((a (stp:find-attribute-named output "indent")))
                  (when a
                    (stp:detach a)))))
            (with-open-file (s out
                               :direction :output
                               :if-exists :rename-and-delete
                               :element-type '(unsigned-byte 8))
              (stp:serialize d (cxml:make-octet-stream-sink s))))
        (error (c)
          (warn "ignoring bogus stylesheet ~A: ~A" in c)
          (copy-file in out)))
      (warn "oops, ignoring missing stylesheet: ~A" in)))

(defun noindent-stylesheet-pathname (test-case)
  (make-pathname :type "noindent-xsl"
                 :defaults (test-stylesheet-pathname test-case)))

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
           (noindent-stylesheet (noindent-stylesheet-pathname test-case))
           #+xuriella::xsltproc
           (out (test-output-pathname test-case "xsltproc"))
           (saxon-out (test-output-pathname test-case "saxon")))
      (sanitize-stylesheet stylesheet noindent-stylesheet)
      (if (equal (test-operation test-case) "standard")
          (handler-case
              (progn
                #+xuriella::xsltproc (xsltproc noindent-stylesheet data out)
                (saxon noindent-stylesheet data saxon-out)
                (report "PASS")
                (write-simplified-test test-case "standard")
                t)
            (error (c)
              (report "FAIL" ": ~A" c)
              (write-simplified-test test-case "execution-error")
              nil))
          (handler-case
              (progn
                #+xuriella::xsltproc
                (xsltproc noindent-stylesheet data "/dev/null")
                (saxon noindent-stylesheet data "/dev/null")
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

(defun saxon (stylesheet input output)
  (flet ((full-namestring (x)
           (namestring (merge-pathnames x))))
    (let* ((asdf::*verbose-out* (make-string-output-stream))
           (code (asdf:run-shell-command
                  "cd ~S && java -jar /usr/share/java/saxon.jar ~S ~S >~S"
                  (full-namestring "")
                  (full-namestring input)
                  (full-namestring stylesheet)
                  (full-namestring output))))
      (unless (zerop code)
        (error "running saxon failed with code ~A [~%~A~%]"
               code
               (get-output-stream-string asdf::*verbose-out*))))))


;;;; RUN-TESTS and DRIBBLE-TESTS

;;; Process katalog.xml

;; temporary configuration until we support enough XSLT that it's worth
;; running all tests:
(defparameter *default-categories*
  ;; '("XSLT-Data-Model" "XPath-Expression" "XPath-Data-Model")
  nil)

(defun dribble-tests (&optional (category *default-categories*)
                      (d *tests-directory*))
  (let ((*package* (find-package 'cl-user))
        (*print-circle* nil))
    (with-open-file (dribble
                     (merge-pathnames "TEST"
                                      (slot-value (asdf:find-system :xuriella)
                                                  'asdf::relative-pathname))
                     :direction :output
                     :if-exists :supersede
                     :external-format :utf-8)
      (let* ((dribble (make-broadcast-stream dribble *standard-output*))
             (*standard-output* dribble)
             (*trace-output* dribble)
             (*error-output* dribble)
             (*terminal-io* (make-two-way-stream *standard-input* dribble)))
        (handler-bind ((warning
                        (lambda (c)
                          (warn "~A" (replace-junk (princ-to-string c)))
                          (muffle-warning c))))
          (run-tests category d))))))

(defparameter *bad-tests*
  '( ;; some tests wants us to recover from this error, yet this one doesn't:
    "copy_copy61"
    "copy_copy62"

    ;; we'd pass these tests, but the test authors forgot to declare the
    ;; entity they're writing, so we can't parse it for comparison.
    "output_output06"
    "output_output10"
    ;; another similar test where the output is unparsable, except that
    ;; here an entity declaration wouldn't have helped either:
    "Copying_ResultTreeFragmentWithEscapedText"

    ;; the following tests take a lot of time due to the problems of current matching algorithm:
    "impincl_impincl16"
    "match_match13"
    ;; stack exhaustion -- matching problem i think
    "Keys_PerfRepro3"
    ;; probably the same problem (but I haven't checked):
    "numbering_numbering03"
    "numbering_numbering10"
    "numbering_numbering11"
    "numbering_numbering80"
    "numbering_numbering81"
    "numbering_numbering94"
    "numbering_numbering95"
    "Import__91164"))

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
                         (and (or (null categories)
                                  (find (test-category test)
                                        categories
                                        :test #'equal))
                              (not (find (test-id test)
                                         *bad-tests*
                                         :test #'equal))))))))

(defun run-named-test (name &optional (d *tests-directory*))
  (klacks:with-open-source
      (source (klacks:make-tapping-source
               (cxml:make-source (merge-pathnames "katalog.xml" d))))
    (let ((*default-pathname-defaults* (merge-pathnames d))
          (*break-on-signals* 'error))
      (map-tests #'run-test
                 source
                 :test (lambda (test) (equal (test-id test) name))))))

(defun copy-file (p q)
  (with-open-file (in p :element-type '(unsigned-byte 8))
    (with-open-file (out q
                         :element-type '(unsigned-byte 8)
                         :direction :output
                         :if-exists :rename-and-delete)
      (let ((buf (make-array 8192 :element-type '(unsigned-byte 8))))
        (loop for pos = (read-sequence buf in)
           until (zerop pos)
           do (write-sequence buf out :end pos))))))

(defun find-named-test (name &optional (d *tests-directory*))
  (klacks:with-open-source
      (source (klacks:make-tapping-source
               (cxml:make-source (merge-pathnames "katalog.xml" d))))
    (block nil
      (map-tests (lambda (test)
                   (return test))
                 source
                 :test (lambda (test) (equal (test-id test) name))))))

(defun copy-test-files (name &optional (d *tests-directory*))
  (let* ((test (find-named-test name d))
         (*default-pathname-defaults* (merge-pathnames d))
         (*break-on-signals* 'error)
         (target-dir (merge-pathnames "copied-test/"
                                      (asdf:component-pathname
                                       (asdf:find-system :xuriella))))
         (xsl (merge-pathnames "test.xsl" target-dir))
         (xml (merge-pathnames "test.xml" target-dir))
         (txt (merge-pathnames "official-output.txt" target-dir))
         (expected (merge-pathnames "expected.xml" target-dir))
         (actual (merge-pathnames "actual.xml" target-dir)))
    (ensure-directories-exist target-dir)
    (copy-file (test-stylesheet-pathname test) xsl)
    (copy-file (test-data-pathname test) xml)
    (when (test-official-output-pathname test)
      (copy-file (test-official-output-pathname test) txt))
    (format t "Test stylesheet copied to:~%  ~A~%~%" xsl)
    (format t "Test data copied to:~%  ~A~%~%" xml)
    (when (test-official-output-pathname test)
      (format t "Official output file:~%  ~A~%~%" txt))
    (format t "Run xsltproc like this:~%  cd ~A~%  xsltproc ~A ~A >~A~%~%"
            (namestring target-dir)
            (enough-namestring xsl target-dir)
            (enough-namestring xml target-dir)
            (enough-namestring expected target-dir))
    (format t "Run saxon like this:~%  cd ~A~%  java -jar /usr/share/java/saxon.jar ~A ~A >~A~%~%"
            (namestring target-dir)
            (enough-namestring xml target-dir)
            (enough-namestring xsl target-dir)
            (enough-namestring expected target-dir))
    (format t "Run MSXSL like this:~%  cd ~A~%  wine msxsl.exe ~A ~A >~A~%~%"
            (namestring target-dir)
            (enough-namestring xml target-dir)
            (enough-namestring xsl target-dir)
            (enough-namestring expected target-dir))
    (format t "Run xuriella like this:~%")
    `(apply-stylesheet ,xsl ,xml :output ,actual)))

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
  (stp:with-attributes (id category operation
                           data stylesheet data-2 stylesheet-2
                           output compare)
      <test-case>
    (make-instance 'test-case
                   :id id
                   :category category
                   :operation operation
                   :data-pathname data
                   :stylesheet-pathname stylesheet
                   :data-pathname-2 data-2
                   :stylesheet-pathname-2 stylesheet-2
                   :output-pathname output
                   :output-compare compare)))

;; read from file P, skipping the XMLDecl or TextDecl and Doctype at the
;; beginning, if any.
(defun slurp-for-comparison (p)
  (with-open-file (s p :element-type '(unsigned-byte 8))
    (unless (and (eql (read-byte s nil) #xef)
                 (eql (read-byte s nil) #xbb)
                 (eql (read-byte s nil) #xbf))
      (file-position s 0))
    (if (plusp (file-length s))
        (slurp-for-comparison-1 p s t)
        "<wrapper/>")))

(defun slurp-for-comparison-1 (p s junk-info)
  (let ((pos (file-position s))         ;for UTF-8 "BOM"
        (xstream (runes:make-xstream s :speed 1))
        (prev-pos 0))
    (setf (runes:xstream-name xstream)
          (cxml::make-stream-name
           :entity-name "main document"
           :entity-kind :main
           :uri (cxml::pathname-to-uri (merge-pathnames p))))
    (let ((source
           (flet ((er (pub sys)
                    pub sys
                    (flexi-streams:make-in-memory-input-stream
                     #())))
             (cxml:make-source xstream
                               :pathname p
                               :entity-resolver #'er))))
      (unless (eq junk-info :nada)
        (loop
           for key = (progn
                       (setf prev-pos (runes:xstream-position xstream))
                       (klacks:peek-next source))
           until (eq key :start-document))
        (cxml::with-source (source cxml::context)
          (when (eq (cxml::zstream-token-category
                     (cxml::main-zstream cxml::context))
                    :NMTOKEN)
            ;; oops, doesn't look like XML at all
            (file-position s pos)
            (return-from slurp-for-comparison-1
              (slurp-for-comparison-1 p s :nada)))))
      (etypecase junk-info
        (integer
         (dotimes (x junk-info)
           (setf prev-pos (runes:xstream-position xstream))
           (klacks:peek-next source)))
        ((eql t)
         (let ((nskip 0))
           (handler-case
               (loop
                  (case (klacks:peek-next source)
                    (:start-element (return))
                    (:characters
                     (if (whitespacep (klacks:current-characters source))
                         (incf nskip)
                         (return)))
                    (t
                     (incf nskip))))
             ((or file-error cxml:xml-parse-error) ()
               (when (zerop nskip)
                 (setf nskip nil))))
           ;; retry
           (with-open-file (u p :element-type '(unsigned-byte 8))
             (file-position u pos)
             (return-from slurp-for-comparison-1
               (slurp-for-comparison-1 p u nskip)))))
        ((member nil :nada)))
      (with-output-to-string (r)
        (let* ((seen-char
                (cxml::with-source (source cxml::context)
                  (ecase (cxml::zstream-token-category
                          (cxml::main-zstream cxml::context))
                    (:seen-< #\<)
                    (:? #\?)
                    ((nil :s) nil))))
               (off-by-one-p (or seen-char (eq junk-info :nada)))
               (new-pos (- prev-pos (if off-by-one-p 1 0))))
          ;; copy doctype over
          (with-open-file (u p :element-type '(unsigned-byte 8))
            (file-position u pos)
            (let ((y (runes:make-xstream u :speed 1)))
              (loop
                 while (< (runes:xstream-position y) new-pos)
                 do (write-char (runes:read-rune y) r))))
          (write-line "<wrapper>" r)
          (when seen-char
            (write-char seen-char r)))
        (loop
           for char = (runes:read-rune xstream)
           until (eq char :eof)
           do (write-char char r))
        (write-line "</wrapper>" r)))))

(defun parse-for-comparison (p)
  (let* ((d (flet ((er (pub sys)
                    pub sys
                    (flexi-streams:make-in-memory-input-stream
                     #())))
              (cxml:parse (slurp-for-comparison p)
                          (make-text-normalizer (stp:make-builder))
                          :entity-resolver #'er)))
         (de (stp:document-element d)))
    (let ((first (stp:first-child de)))
      (when (typep first 'stp:text)
        (cond
          ((whitespacep (stp:data first))
           (stp:delete-child first de))
          (t
           (setf (stp:data first)
                 (cl-ppcre:regex-replace #.(format nil "^[~A]+" *whitespace*)
                                         (stp:data first)
                                         ""))))))
    (let ((last (stp:last-child de)))
      (when (typep last 'stp:text)
        (cond
          ((whitespacep (stp:data last))
           (stp:delete-child last de))
          (t
           (setf (stp:data last)
                 (cl-ppcre:regex-replace #.(format nil "[~A]+$" *whitespace*)
                                         (stp:data last)
                                         ""))))))
    d))

(defun output-equal-p (compare p q &key normalize)
  (handler-case
      (ecase compare
        (:xml (xml-output-equal-p p q normalize))
        (:html (html-output-equal-p p q))
        (:text (text-output-equal-p p q)))
    ((or error parse-number::invalid-number) (c)
      (warn "comparison failed: ~A" c)
      nil)))

;; Workaround for namespace_namespace23 and other tests:
;;  - For these tests, saxon and msxsl output a declaration for the XSL
;;    namespace without using that declaration.
;;  - I think saxon and msxsl are both wrong.
;;  - The official test output agrees with my assessment.
;;    (So does libxslt, but that's not to be trusted. :-))
;;  - Here's the catch: The official test output is broken in its whitespace
;;    handling.
;; So let's normalize spaces in test output that looks like an XSLT
;; stylesheet, allowing us to pass these tests using the official test output.
(defun maybe-normalize-test-spaces (wrapper force)
  (stp:do-children (wrapper-child wrapper)
    (when (and (typep wrapper-child 'stp:element)
               (or (equal (stp:namespace-uri wrapper-child) *xsl*)
                   force))
      (strip-stylesheet wrapper-child)
      (labels ((recurse (e &optional preserve)
                 (stp:do-children (child e)
                   (typecase child
                     (stp:text
                      (setf (stp:data child)
                            (normalize-whitespace (stp:data child))))
                     (stp:element
                         (stp:with-attributes ((space "space" *xml*))
                             child
                           (let ((new-preserve
                                  (cond
                                    ((namep child "text") t)
                                    ((not space) preserve)
                                    ((equal space "preserve") t)
                                    (t nil))))
                             (recurse child new-preserve))))))))
        (recurse wrapper-child)))))

(defun xml-output-equal-p (p q normalize)
  (let ((r (parse-for-comparison p))
        (s (parse-for-comparison q)))
    (maybe-normalize-test-spaces (stp:document-element r) normalize)
    (maybe-normalize-test-spaces (stp:document-element s) normalize)
    (node= (stp:document-element r) (stp:document-element s))))

;; FIXME: don't do this in <pre> etc.
(defun normalize-html-whitespace (node)
  (when (typep node 'stp:parent-node)
    ;; ignore newlines after start tags completely
    (let ((first (stp:first-child node)))
      (when (and (typep first 'stp:text)
                 (alexandria:starts-with #\newline (stp:data first)))
        (setf (stp:data first) (subseq (stp:data first) 1))))
    ;; ignore newlines before end tags completely
    (let ((last (stp:last-child node)))
      (when (and (typep last 'stp:text)
                 (alexandria:ends-with #\newline (stp:data last)))
        (setf (stp:data last)
              (subseq (stp:data last) 0 (length (stp:data last))))))
    ;; normalize sequences of whitespace
    (stp:do-children (child node)
      (if (typep child 'stp:text)
          (setf (stp:data child)
                (let ((str (normalize-whitespace (stp:data child))))
                  (when
                      ;; FIXME!  Here we remove whitespace entirely.
                      ;; Totally incorrect, but I don't see how we could
                      ;; watch Saxon's output otherwise.
                      (equal str " ")
                    (setf str ""))
                  str))
          (normalize-html-whitespace child)))
    ;; just to be sure, join adjacent nodes
    (cxml-stp-impl::normalize-text-nodes! node)))

;; FIXME: this check is too lenient, because chtml is an error-correcting
;; parser.
(defun html-output-equal-p (p q)
  (let ((r (chtml:parse (pathname p) (stp:make-builder)))
        (s (chtml:parse (pathname q) (stp:make-builder))))
    (normalize-html-whitespace r)
    (normalize-html-whitespace s)
    (node= (stp:document-element r) (stp:document-element s))))

(defun text-output-equal-p (p q)
  (with-open-file (a p :element-type '(unsigned-byte 8))
    (with-open-file (b q :element-type '(unsigned-byte 8))
      (let ((len (file-length a)))
        (and (eql len (file-length b))
             (let ((d (make-array len :element-type '(unsigned-byte 8)))
                   (e (make-array len :element-type '(unsigned-byte 8))))
               (read-sequence d a)
               (read-sequence e b)
               (equalp d e)))))))

(defun strip-addresses (str)
  (cl-ppcre:regex-replace-all "{[0-9a-fA-F]+}\\>" str "{xxxxxxxx}>"))

(defun slurp-output-method (p)
  (xpath:with-namespaces ((nil #.*xsl*))
    (let* ((d (handler-bind
                  ((warning #'muffle-warning))
                (cxml:parse (pathname p) (stp:make-builder))))
           (output (xpath:first-node (xpath:evaluate "//output" d))))
      (if output
          (let ((method (stp:attribute-value output "method")))
            (if method
                (intern (string-upcase method) :keyword)
                :xml))
          :xml))))

(defun replace-junk (str)
  (cl-ppcre:regex-replace-all
   `(:group ,(namestring *tests-directory*))
   (map 'string
        (lambda (c)
          (if (or (eql c #\newline) (<= 32 (char-code c) 126))
              c
              #\?))
        str)
   "..."))

(defun run-test (test)
  (let ((expected-saxon (test-output-pathname test "saxon"))
        #+xuriella::xsltproc
        (expected-xsltproc (test-output-pathname test "xsltproc"))
        (actual (test-output-pathname test "xuriella"))
        (official (test-official-output-pathname test))
        (force-normalization
         (find (test-id test) '("Namespace-alias__91782") :test #'equal))
        (output-method nil))
    (handler-bind ((|hey test suite, this is an HTML document|
                    (lambda (c)
                      (declare (ignore c))
                      (setf output-method :html))))
      (labels ((uri-resolver (uri)
                 (if (search "%5c%5c%5c%5cwebxtest%5c%5cmanagedshadow%5c%5cmanaged_b2%5c%5ctestdata%5c%5cxslt%5c%5celement%5c%5cxslt_element_NSShared.xml"
                             uri)
                     (cxml::pathname-to-uri
                      (merge-pathnames
                       "MSFT_Conformance_Tests/Elements/xslt_element_NSShared.xml"
                       *tests-directory*))
                     uri))
               (doit ()
                 (with-open-file (s actual
                                    :if-exists :rename-and-delete
                                    :direction :output
                                    :element-type '(unsigned-byte 8))
                   (handler-bind ((xslt-error
                                   (lambda (c)
                                     (declare (ignore c))
                                     (when (find-restart 'recover)
                                       (invoke-restart 'recover)))))
                     (apply-stylesheet (pathname (test-stylesheet-pathname test))
                                       (pathname (test-data-pathname test))
                                       :output s
                                       :uri-resolver #'uri-resolver))))
               (pp (label pathname)
                 (when pathname
                   (format t "  ~A: ~A~%"
                           label
                           (enough-namestring pathname *tests-directory*))))
               (report (ok &optional (fmt "") &rest args)
                 (write-string
                  (replace-junk
                   (strip-addresses
                    (format nil "~&~:[FAIL~;PASS~] ~A [~A]~?~%"
                            ok
                            (test-id test)
                            (test-category test)
                            fmt
                            args))))
                 (pp "Stylesheet" (test-stylesheet-pathname test))
                 (pp "Data" (test-data-pathname test))
                 (pp "Supplemental stylesheet"
                     (test-stylesheet-pathname-2 test))
                 (pp "Supplemental data" (test-data-pathname-2 test))
                 (pp "Expected output (1)" expected-saxon)
                 #+xuriella::xsltproc
                 (pp "Expected output (2)" expected-xsltproc)
                 (pp "Actual output" actual)
                 (terpri)
                 ok))
        (cond
          ((equal (test-operation test) "standard")
           (handler-case
               (progn
                 (when (find (test-id test)
                             nil ;;'("axes_axes47" "attribset_attribset20")
                             :test #'equal)
                   (error "skipping problematic test"))
                 (doit)
                 (let* ((output-method
                         (or output-method
                             (slurp-output-method
                              (test-stylesheet-pathname test))))
                        (saxon-matches-p
                         (output-equal-p output-method
                                         expected-saxon
                                         actual))
                        #+xuriella::xsltproc
                        (xsltproc-matches-p
                         (output-equal-p output-method
                                         expected-xsltproc
                                         actual))
                        (official-matches-p
                         (output-equal-p output-method
                                         official
                                         actual
                                         :normalize force-normalization)))
                   (cond
                     ((or saxon-matches-p
                          #+xuriella::xsltproc xsltproc-matches-p
                          official-matches-p)
                      (report t)
                      #+xuriella::xsltproc
                      (report t ": saxon ~A, xsltproc ~A~:[~; (MISMATCH)~]"
                              saxon-matches-p
                              xsltproc-matches-p
                              (if saxon-matches-p
                                  (not xsltproc-matches-p)
                                  xsltproc-matches-p)))
                     (t
                      (report nil ": output doesn't match")))))
             ((or error parse-number::invalid-number) (c)
               (report nil ": ~A" c))))
          (t
           (handler-case
               (doit)
             (xslt-error (c)
               (report t ": raised an xslt-error as expected" c))
             ((or error parse-number::invalid-number) (c)
               (report nil ": condition of incorrect type: ~%~A" c))
             (:no-error (result)
               (cond
                 ((not (and official (probe-file official)))
                  (report nil ": expected error not signalled: " result))
                 ((output-equal-p
                   (or output-method
                       (slurp-output-method (test-stylesheet-pathname test)))
                   official
                   actual
                   :normalize force-normalization)
                  (report t))
                 (t
                  (report nil ": saxon error not signalled and official output not a match")))))))))))

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
       ;; (equal (stp:namespace-prefix a) (stp:namespace-prefix b))
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
       (block nil
         (flet ((check-namespaces (a b)
                  (let ((result ()))
                    (stp:map-extra-namespaces
                     (lambda (k v)
                       (unless (equal v (stp:find-namespace k b))
                         (return nil)))
                     a)
                    result)))
           (check-namespaces e f)
           (check-namespaces f e))
         t)))

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

(xpath-sys:define-xpath-function/eager
    xslt :print
    (thing)
  (if (xpath:node-set-p thing)
      (loop
         initially (format t ";;; node set:~%")
         for i from 0
         for node in (xpath:all-nodes thing)
         do
           (format t ";;;   ~D: ~A~%" i (type-of node)))
      (format t ";;; ~A~%" thing))
  thing)
