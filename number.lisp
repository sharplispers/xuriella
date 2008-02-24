;;; -*- show-trailing-whitespace: t; indent-tabs: nil -*-

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
            (or value
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
     (:path (:ancestor-or-self :node) ,@(cdr (parse-pattern str))))
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
          (let ((qname (xpath-protocol:qualified-name node)))
            (lambda (ctx)
              (let ((node (xpath:context-node ctx)))
                (xpath:make-node-set
                 (if (equal (xpath-protocol:qualified-name node) qname)
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
       (let ((nodes (xpath::force (funcall (xpath::axis-function :descendant-or-self) root))))
         (when from
           (loop
              for (current . rest) on nodes
              until (pattern-thunk-matches-p from current)
              finally (setf nodes rest)))
         (list
          (loop
             for node in nodes
             while (pattern-thunk-matches-p count node)
             count t)))))
    (t
     (xslt-error "invalid number level: ~A" level))))

(defun format-number-list
    (list format lang letter-value grouping-separator grouping-size)
  (declare (ignore lang letter-value))
  (if (equal format "1")
      (format nil "~{~D~^.~}" list)
      (error "sorry, format-number-list not implemented yet")))
