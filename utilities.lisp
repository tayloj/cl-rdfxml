;;; Copyright (c) 2008, Joshua Taylor.  All rights reserved.

;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:

;;;   * Redistributions of source code must retain the above copyright
;;;     notice, this list of conditions and the following disclaimer.

;;;   * Redistributions in binary form must reproduce the above
;;;     copyright notice, this list of conditions and the following
;;;     disclaimer in the documentation and/or other materials
;;;     provided with the distribution.

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

(in-package #:cl-rdfxml)

;;;; String Concatenation

(defun strcat (&rest strings)
  "strcat string* => string
strings, string---strings
strcat returns the concatentation of the strings."
  (with-output-to-string (*standard-output*)
    (dolist (string strings)
      (write-string (string string)))))

;;;; Alist iteration

(defmacro doalist (((key value &optional (pair (make-symbol "PAIR")))
                    list
                    &optional (result nil))
                   &body body)
  "doalist ((key value [pair]) list-form [result-form]) {tag | statement}* => result*
key, value, pair---symbols
list-form, result-form,, statement---forms
tag---a go tag; not evaluated
results---if a return or return from form is executed, the values
passed from that form\; otherwise, the values returned by the
result-form or nil if there is no result form.
Doalist iterates over the keys and values (and conses) in an
association list. The body of doalist is like a tagbody. doalist is
similar to a dolist, but rather than binding a single variable to each
element of a list, doalist binds key and value to the car and cdr of
each cons in an association list, and pair to the cdr."
  (let ((declarations '()))
    ;; Pull all of the declarations from the beginning of body and
    ;; save them in declarations. Then when we hit a non-declaration,
    ;; we've saved the declarations, and we know what the proper body
    ;; ought to be.
    (loop for form in body
          while (and (consp form)
                     (eq (car form) 'declare))
          do (push form declarations)
          do (setf body (cdr body)))
    `(dolist (,pair ,list ,result)
       (destructuring-bind (,key . ,value) ,pair
         ,@(nreverse declarations)
         (tagbody ,@body)))))

;;;; Prompts

(defun prompt-for-line (message &rest args)
  "prompt-for-line message &rest args => line, okp
message---a format control
args---format arguments for message
line---a string
okp---a boolean
Prompt-for-line displays the prompt specified by message and args on
*query-io* and reads a line from *query-io*. If the reading is
successful, the line is returned and okp is true. If reading fails,
say, from an EOF, then line is the empty string, and okp is false."
  (fresh-line *query-io*)
  (apply 'format *query-io* message args)
  (write-char #\space *query-io*)
  (let ((line (read-line *query-io* nil nil)))
    (if (null line)
      (values "" nil)
      (values line t))))

(defun prompt-for-uri ()
  "prompt-for-uri => result
result---a list of two values.
prompt-for-uri returns the list of values that result from invoking
prompt-for-line with the message \"Enter a URI:\"."
  (multiple-value-list
   (prompt-for-line "Enter a URI:")))

#+:lispworks
(editor:setup-indent "define-constant" 1)
