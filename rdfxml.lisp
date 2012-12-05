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

;;; RDF URI classes

(declaim (inline core-syntax-term-p syntax-term-p old-term-p
                 node-element-uri-p property-element-uri-p
                 property-attribute-uri-p))

(defun core-syntax-term-p (object)
  "core-syntax-term-p object => boolean
core-syntax-term-p returns true if object is in +core-syntax-terms+."
  (or (eq object +rdf-rdf+)
      (eq object +rdf-id+)
      (eq object +rdf-about+)
      (eq object +rdf-parse-type+)
      (eq object +rdf-resource+)
      (eq object +rdf-node-id+)
      (eq object +rdf-datatype+)))

(defun syntax-term-p (object)
  "syntax-term-p object => boolean
syntax-term-p returns true if object is a syntax term (a core syntax
term, rdf:Description, or rdf:li)."
  (or (core-syntax-term-p object)
      (eq object +rdf-description+)
      (eq object +rdf-li+)))

(defun old-term-p (object)
  "old-term-p object => boolean
old-term-p returns true if object is in +old-terms+."
  (or (eq object +rdf-about-each+)
      (eq object +rdf-about-each-prefix+)
      (eq object +rdf-bag-id+)))

(defun node-element-uri-p (xuri &aux (uri (intern-uri xuri)))
  "node-element-uri-p xuri => boolean
xuri---a URI designator
Node-element-uri-p determines whether the designated URI is a valid
node element URI. Node element URIs are all URIs except core syntax
terms, rdf:li, and old RDF terms."
  (and (not (core-syntax-term-p uri))
       (not (eq uri +rdf-li+))
       (not (old-term-p uri))))

(defun property-element-uri-p (xuri &aux (uri (intern-uri xuri)))
  "property-element-uri-p xuri => boolean
xuri---a URI designator
Property-element-uri-p determines whether the designated URI is a
valid property element URI. Property element URIs are all URIs except
core syntax terms, rdf:Description, and old RDF terms."
  (and (not (core-syntax-term-p uri))
       (not (eq uri +rdf-description+))
       (not (old-term-p uri))))

(defun property-attribute-uri-p (xuri &aux (uri (intern-uri xuri)))
  "property-attribute-uri-p xuri => boolean
xuri---a URI designator
Property-attribute-uri-p determines whether the designated URI is a
valid property attribute URI. Property attribute URIs are all URIs
except core syntax terms, rdf:Description, rdf:li, and old RDF terms."
  (and (not (core-syntax-term-p uri))
       (not (eq uri +rdf-description+))
       (not (eq uri +rdf-li+))
       (not (old-term-p uri))))

(defun rdf-namespace-prefixed-p (xuri)
  "rdf-namespace-prefixed xuri => boolean
xuri---a uri designator
Rdf-namespace-prefixed-p returns true if xuri is prefixed with
+rdf-namespace+."
  (let ((uri (puri:intern-uri xuri)))
    (prefixp +rdf-namespace+ (puri::uri-string uri))))

(defun membership-property-element-p (xuri)
  "membership-property-element-p xuri => boolean
xuri---a uri designator
Membership-property-element-p returns true if the designated URI is
either rdf:li or of the form rdf:_n where n standard for a sequence of
base-10 digits."
  (let ((uri (puri:intern-uri xuri)))
    (or (eq uri +rdf-li+)
        (and (rdf-namespace-prefixed-p uri)
             (membership-fragment-p (puri:uri-fragment uri))))))

(defun membership-fragment-p (fragment)
  "membership-fragment fragment => boolean
fragment---a string or NIL
Membership-fragment-p returns true if fragment is of the form \"_n
where n is a decimal integer greater than zero with no leading
zeros.\" (RDF/XML Syntax Specification, Section 5.1)"
  (if (null fragment) nil
    (let ((fragment fragment))
      (declare (string fragment))
      (and (>= (length fragment) 2)
           (char= (char fragment 0) #\_)
           (char/= (char fragment 1) #\0)
           (every 'digit-char-p (subseq fragment 1))))))

(defun rdf-name-p (xuri)
  "rdf-name-p xuri => boolean
xuri---a URI designator
Rdf-name-p returns true if the designated URI is an RDF name,
including the infinite set of rdf:_n names."
  (let ((uri (puri:intern-uri xuri)))
    ;; This list comes from 5.1 "The RDF Namespace and Vocabulary in
    ;; the RDF/XML Syntax Specification"
    (flet ((syntax-name-p (uri)
             (or (eq uri +rdf-rdf+)
                 (eq uri +rdf-description+)
                 (eq uri +rdf-id+)
                 (eq uri +rdf-about+)
                 (eq uri +rdf-parse-type+)
                 (eq uri +rdf-resource+)
                 ;; rdf:li 
                 (eq uri +rdf-node-id+)
                 (eq uri +rdf-datatype+)))
           (class-name-p (uri)
             (or (eq uri +rdf-seq+)
                 (eq uri +rdf-bag+)
                 (eq uri +rdf-alt+)
                 (eq uri +rdf-statement+)
                 (eq uri +rdf-property+)
                 (eq uri +rdf-xml-literal+)
                 (eq uri +rdf-list+)))
           (property-name-p (uri)
             (or (eq uri +rdf-subject+)
                 (eq uri +rdf-predicate+)
                 (eq uri +rdf-object+)
                 (eq uri +rdf-type+)
                 (eq uri +rdf-value+)
                 (eq uri +rdf-first+)
                 (eq uri +rdf-rest+)
                 ;; rdf:_n
                 ))
           (resource-name-p (uri)
             (eq uri +rdf-nil+)))
      (or (syntax-name-p uri)
          (class-name-p uri)
          (property-name-p uri)
          (resource-name-p uri)
          ;; rdf:li and rdf:_n
          (membership-property-element-p uri)))))

;;;; PURI Utilities

;; PURI utilities appear in a strange place, because we want to be
;; able to signal warnings when URIs are encountered that begin with
;; the RDF namespace, but are not actually RDF names. To do this, we
;; need the RDF name utilities defined above.

(defun intern-uri (xuri)
  "intern-uri xuri => uri
xuri---a uri designator
uri---a puri:uri
Intern URI is a wrapper around puri:intern-uri that provides a
use-value restart, so that a new uri designator can be provided."
  (let ((uri (restart-case (puri:intern-uri xuri)
               (parse-uri-non-strictly ()
                 :report "Try a non-strict parse."
                 :test (lambda (condition)
                         (and puri:*strict-parse*
                              (typep condition 'puri:uri-parse-error)))
                 (let ((puri:*strict-parse* nil))
                   (intern-uri xuri)))
               (use-value (value okp)
                 :report "Use a different URI."
                 :interactive prompt-for-uri
                 (if okp
                   (intern-uri value)
                   (intern-uri xuri))))))
    (prog1 uri
      (when (and (rdf-namespace-prefixed-p uri)
                 (not (rdf-name-p uri)))
        (warn 'rdf-prefixed-non-rdf-name :name uri)))))

(defun merge-uris (uri base)
  "merge-uris uri base => interned-uri
uri, base---uri designators
interned-uri---a uri
merge-uris is a convenience function that merges uri and base and
interns the result."
  (intern-uri 
   (restart-case (puri:merge-uris uri base)
     (parse-uri-non-strictly ()
       :report "Try a non-strict parse."
       :test (lambda (condition)
               (and puri:*strict-parse*
                    (typep condition 'puri:uri-parse-error)))
       (let ((puri:*strict-parse* nil))
         (merge-uris uri base)))
     (use-value (value okp)
       :report "Supply a different uri to merge with base."
       :interactive prompt-for-uri
       (if okp 
         (merge-uris value base)
         (merge-uris uri base)))
     (use-value (value okp)
       :report "Use a different URI."
       :interactive prompt-for-uri
       (if okp
         (intern-uri value)
         (merge-uris uri base))))))

(defun add-fragment (id base)
  "add-fragment id base => interned-uri
id---a uri fragment id
base---a uri designator
Add-fragment merges \"#<id>\" with base, and returns the result of
interning the merged URI."
  (merge-uris (strcat #\# id) base))

;;;; RDF Concepts

;;; Blank Nodes

(defclass blank-node ()
  ((id
    :initarg :id
    :type string
    :reader blank-node-id
    :documentation "The graph-local ID of the blank-node. This slot is
unbound if the blank node has no id. The id of a blank node is used to
identify the blank node within the scope of a particular graph. That
two blank-nodes happen to have the same ID is not an indication that
they represent the same blank node."))
  (:documentation "The blank-node class represents blank nodes in a
graph. Blanks nodes are local to a graph, and can be identified within
a graph by their ID. The blank-node id is used for refering to the
same blank node in an RDF/XML document, and so in general, blank-nodes
ought to compared using object equality, i.e., eq."))

(defgeneric blank-node-id (blank-node)
  (:documentation "blank-node-id blank-node => id
blank-node---a blank-node
id---a string
Blank-node-id returns the ID of the blank-node. Blank-node-ids are
intended to be used for readability purposes. Blank-nodes should be
compared directly using object equality. That two blank-nodes have ids
that are string= does not mean that they represent the same RDF
blank-node."))

(defmethod print-object ((bnode blank-node) stream)
  "print-object bnode stream => bnode
bnode---a blank-node
stream---an output stream
Print-objects prints a representation of the blank node to stream. The
representation is not readable."
  (prog1 bnode
    (if (not (slot-boundp bnode 'id))
      (print-unreadable-object (bnode stream :identity t :type t))
      (print-unreadable-object (bnode stream :type t)
        (write-string (blank-node-id bnode) stream)))))

(defvar *blank-nodes* nil
  "An equal hash table that is used for `interning' blank node IDs
within the scope of a single graph. Initially nil, but should be bound
for each graph being processed.")

(defun blank-node (&optional (id nil idp) (namespace *blank-nodes*))
  "blank-node [id [namespace]] => blank-node
id---a string
namespace---an equal hash-table
Blank-node returns a blank node. If id is specified, then if there is
already a blank node in namespace whose id is equal to id, then that
blank node is returned. Otherwise, a new blank node is created,
inserted into namespace, and returned. If id is not specified, then a
new blank node is returned, and namespace is not modified."
  (if (not idp) (make-instance 'blank-node)
    (multiple-value-bind (bnode present?) (gethash id namespace)
      (if present? bnode
        (setf (gethash id namespace)
              (make-instance 'blank-node :id id))))))

;;; Literals

(defclass literal ()
  ((string
    :initarg :string
    :reader literal-string
    :type string
    :documentation "The lexical form of the literal."))
  (:documentation "The literal class is the superclass of both the
plain-literal and the typed literal. Every literal has some lexical
form, and the slot storing this form is defined in the literal class,
and may be read with literal-string."))

(defgeneric literal-string (literal)
  (:documentation "literal-string literal => string
literal---a literal
string---a string
Literal-string returns the lexical form of the literal."))

;; plain-literals

(defclass plain-literal (literal)
  ((language
    :initarg :language
    :reader literal-language
    :type (or null string)
    :documentation "The language tag associated with a plain literal.
If language is nil, the the plain-literal has no language type. If
language is non-nil, it should be a string that conforms to RDF 3066."))
  (:documentation "The plain-literal class is the class comprising all
plain-literals. These literals have a lexical form, inherited from the
superclass literal, and an optional language tag. The language tag,
when provided, should be of the form specified by RFC 3066, and is
normalized to lowercase."))

(defgeneric literal-language (plain-literal)
  (:documentation "literal-language plain-literal => result
plain-literal---a plain-literal
result---a string or nil
Literal-language return the language tag of the plain-literal, if
there is one, and nil if no language tag is associated with the
literal."))

(defmethod print-object ((literal plain-literal) stream)
  "print-object plain-literal stream => plain-literal
plain-literal---a plain-literal
stream---an output stream
Print-object prints the plain-literal in a form similar to the W3C RDF
Validation output. The literal string appears with double quotation
marks, and the language-tag, if provided is output with an asperand."
  (prog1 literal
    (print-unreadable-object (literal stream)
      (write (literal-string literal) :stream stream)
      (unless (null (literal-language literal))
        (write-char #\@ stream)
        (write-string (literal-language literal) stream)))))

(defvar *plain-literals* (make-hash-table :test 'equal)
  "An equal hash-table used for interning plain literals, that is,
literals with a string component and an optional language tag.")

(defun intern-plain-literal (string &optional (language nil))
  "intern-plain-literal string [language] => plain-literal
string, language---strings
plain-literal---a plain literal
Intern-plain-literal returns a literal with the specified string and
language. Calls to intern-plain-literal with strings that are equal
and languages that are equal return the same literal object."
  (let* ((language (etypecase language
                     (null language)
                     (string (string-downcase language))))
         (key (cons string language)))
    (multiple-value-bind (literal present?)
        (gethash key *plain-literals*)
      (if present? literal
        (setf (gethash key *plain-literals*)
              (make-instance 'plain-literal
                             :string string
                             :language language))))))

;; typed-literals

(defclass typed-literal (literal)
  ((datatype
    :initarg :datatype
    :reader literal-datatype
    :type puri:uri
    :documentation "The datatype of a typed-literal, which is not
optional, is a URI designating the datatype."))
  (:documentation "The typed-literal class is the class comprising all
typed-literals. These literals have a lexical form, inherited from the
superclass literal, and a required datatype. The datatype is a
puri:uri."))

(defgeneric literal-datatype (typed-literal)
  (:documentation "literal-datatype typed-literal => datatype
typed-literal---a typed-literal
datatype---an interned PURI uri
Literal-datatype returns the datatype of a typed-literal. The datatype
URI is interned, and may be compared with eq."))

(defmethod print-object ((literal typed-literal) stream)
  "print-object typed-literal stream => typed-literal
typed-literal---a typed-literal
stream---an output stream
Print-object prints the typed-literal in a form similar to the W3C RDF
Validation output. The literal string appears with double quotation
marks, and the datatype URI is printed with a ^^ prefix."
  (prog1 literal
    (print-unreadable-object (literal stream)
      (write (literal-string literal) :stream stream)
      (write-string "^^" stream)
      (puri:render-uri (literal-datatype literal) stream))))

(defvar *typed-literals* (make-hash-table :test 'equal)
  "An equal hash-table used for interning typed literals, that is,
literals with a string component and a datatype.")

(defun intern-typed-literal (string datatype)
  "intern-plain-literal string datatype => typed-literal
string---a string
datatype---a URI designator
typed-literal---a typed literal
intern-typed-literal returns a literal with the specified string and
datatype. Calls to intern-plain-literal with strings that are equal
and designators for the same URI return the same literal object."
  (let* ((datatype (intern-uri datatype))
         (key (cons string datatype)))
    (multiple-value-bind (literal present?)
        (gethash key *typed-literals*)
      (if present? literal
        (setf (gethash key *typed-literals*)
              (make-instance 'typed-literal
                             :string string
                             :datatype datatype))))))

;;;; XML Utilities

(defvar +xml-namespace+
  "http://www.w3.org/XML/1998/namespace"
  "The string form of the URI XML namespace.")

(defun id-name-p (string)
  "id-name-p string => boolean
string---a string
Id-name-p returns true if string is a valid XML NCName, which are the
only valid names attribute values for rdf:ID and rdf:nodeID."
  (declare (string string))
  (and (cxml::valid-name-p string)
       ;; cxml::nc-name-p assumes string is a valid name.
       (cxml::nc-name-p string)))

;;; Whitespace

(declaim (inline xml-whitespace-p))
(defun xml-whitespace-p (string)
  "xml-whitespace-p string => boolean
string---a string-designator
xml-whitespace-p returns true if every element of the string is an XML
whitespace character (i.e., is #x20, #x9, #xD, or #xA), or if string is
the empty string."
  (loop for char across (string string)
        for code = (char-code char)
        always (or (eql code #x20)
                   (eql code #x9)
                   (eql code #xD)
                   (eql code #xA))))

(defun peek-skipping-comments (source)
  "peek-skipping-comments source => result*
souce---a cxml:source
Peek-skipping-comments returns the same values that klacks:peek
returns, with the exception that if the next event from source is
:comments, the event is consumed. The values of the first non-comment
event are returned."
  ;; thanks to Red Daly, reddaly@gmail.com
  (loop while (eq :comment (klacks:peek source))
        do (klacks:consume source)
        finally (return (klacks:peek source))))

(defun consume-whitespace (source)
  "consume-whitespace source => |
source---a cxml source
Consume-whitespace peeks and consumes events from source while the
events are of type :characters and the associated string satisfies
xml-whitespace-p, or if the event is :comment."
  (declare (optimize (speed 3) (safety 0)))
  (loop for event = (klacks:peek source)
        while (or (and (eq :characters event)
                       (xml-whitespace-p (klacks:peek-value source)))
                  (eq :dtd event)
                  (eq :start-document event)
                  (eq :comment event))
        do (klacks:consume source)))

;;; Element and Attribute Utilities

(defun element-uri (source)
  "element-uri source => uri
source---a cxml:source
uri---a URI
Element-uri returns the result of interning the concatenation of the
current element's uri and lname. Element-uri calls klacks:current-uri
and klacks:current-lname, which signal an error if the current event is
not :start-element or :end-element."
  (intern-uri
   (strcat (klacks:current-uri source)
           (klacks:current-lname source))))

(defun prefixp (prefix sequence &key (test 'eql))
  "prefixp prefix sequence => boolean, properp
prefix, string---sequences
properp---a boolean
prefixp returns true if prefix is a prefix of sequence. Elements of
prefix and sequence are compared using test. A second value, properp,
is true when the length of the prefix is less than the length of the
sequence, regardless of whether prefix is actually a prefix of
sequence."
  (let ((lp (length prefix))
        (ls (length sequence)))
    (values (and (every test prefix sequence)
                 (<= lp ls))
            (< lp ls))))

(defun xml-lang-attribute-p (attribute)
  "xml-lang-attribute-p attribute => boolean
attribute---a sax:attribute
xml-lang-attribute-p returns true if attribute has a local-name
\"name\" and a namespace-uri +xml-namespace+."
  (and (string= "name" (sax:attribute-local-name attribute))
       (string= +xml-namespace+ (sax:attribute-namespace-uri attribute))))

(defun xml-attribute-p (attribute)
  "xml-attribute-p attribute => boolean
attribute---a sax:attribute
Xml-attribute-p returns true if attribute has a prefix that begins
with \"xml\", or if the attribute has no prefix and the attribute's
local name begins with \"xml\". or if attribute satisfies
xml-lang-attribute-p."
  (or (xml-lang-attribute-p attribute)
      (prefixp "xml" (sax:attribute-qname attribute) :test 'char-equal)))

(defun attribute-uri (attribute source)
  "attribute-uri attribute source => uri
attribute---a sax:attribute
source---a cxml:source
uri---a PURI uri
Attribute-uri returns the URi associated with the attribute, as
described by Section 6.1.4 Attribute Event in the RDF/XML Syntax
specification. In general this is the concatenation of the namespace
URI with the local name. If no namespace is provided, and the local
name is ID, about, resource, parseType, or type, then the
corresponding RDF term URI is returned. Otherwise, an error is
signalled. "
  (let ((local-name (sax:attribute-local-name attribute))
        (namespace (sax:attribute-namespace-uri attribute)))
    (cond
     ((not (null namespace))
      (intern-uri (strcat namespace local-name)))
     ((find local-name #("ID" "about" "resource" "parseType" "type")
            :test 'string=)
      (warn 'non-namespaced-name :name local-name)
      (intern-uri (strcat +rdf-namespace+ local-name)))
     (t (error 'non-namespaced-attribute
               :attribute local-name
               :source source)))))

(defvar *element-ids* nil
  "During parsing, an 'equal hash table that functions as a set of the
IDs that have been processed. Duplicate IDs in an RDF/XML document are
prohibited, i.e., it is an error for two elements to have the same
value on an rdf:ID attribute.")

(defun unique-attributes (attributes source)
  "unique-attributes attributes source => uniques
attributes, uniques---associaation lists
source---a cxml:source
Unique-attributes returns an association list that no entries with the
same key. The entries of the new association list are those of the
original association list, but in the event that a duplicate is
detected, an error of type duplicate-attribute is signalled, and
various options are provided."
  (let ((ignored-attributes '())
        (new-attributes '()))
    ;; For each entry in the attributes, if the attribute is not being
    ;; ignored, check whether a value for the attribute has already
    ;; been provided. If it has not, then save the value. Otherwise,
    ;; signal an error of type duplicate-attribute. Restarts are:
    ;; ignore /all/ occurances of this attribute in attributes
    ;; (including the ones seen earlier), selecting the old value, and
    ;; selecting the new value.
    (doalist ((uri value pair) attributes new-attributes)
      (unless (member uri ignored-attributes)
        (let ((old-value (assoc uri new-attributes)))
          (if (null old-value) (push pair new-attributes)
            (restart-case (error 'duplicate-attribute
                                 :attribute uri :source source)
              (ignore-attribute ()
                :report "Ignore this attribute."
                (push uri ignored-attributes)
                (setf new-attributes (delete old-value new-attributes)))
              (use-old-value ()
                :report
                (lambda (o)
                  (format o "Use old attribute value ~S." (cdr old-value))))
              (use-new-value ()
                :report
                (lambda (o)
                  (format o "Use new attribute value ~S." value))
                (setf (cdr old-value) value)))))))))

(defun ensure-non-repeated-id (attributes source)
  "ensure-non-repeated-id attributes => new-attributes
attributes, new-attributes---association lists
Ensure-non-repeated-id ensures that if the association list attributes
contains a value for the key rdf:ID, that *element-ids* does not
currently map value to t, that is, that the element has not appeared
on another element. If there is an rdf:ID value and it has already
been specified on another element, restarts include using this value
anyway, ignoring the rdf:ID attribute, and using a different value. In
any of the cases that an ID is eventually specified, *element-ids* is
updated to include the new ID."
  (let ((pair (assoc +rdf-id+ attributes)))
    (if (null pair) attributes
      (do () ((not (gethash (cdr pair) *element-ids*))
              (setf (gethash (cdr pair) *element-ids*) t)
              attributes)
        (restart-case (error 'repeated-id
                             :attribute +rdf-id+
                             :value (cdr pair)
                             :source source)
          (nil ()
            :report "Use it anyway."
            (return attributes))
          (ignore-attribute ()
            :report "Ignore it on this element."
            (return (delete pair attributes)))
          (use-value (new-value okp)
            :report "Use a different ID."
            :interactive (lambda ()
                           (multiple-value-list
                            (prompt-for-line "Enter an ID:")))
            (when okp (setf (cdr pair) new-value))))))))

(defun element-attributes (source)
  "element-attributes source => attributes
source---a cxml:source
attributes---an alist
Element-attributes returns an association list whose keys are the
attribute URIs of the attributes of the current element and whose
values are the corresponding values. The attributes used in RDF/XML
are the atributes of the element, except that xml attributes are
removed (those satisfying xml-attribute-p) according to Section 6.1.2
\"Element Event\" in the RDF/XML Syntax Specification."
  (let* ((attributes (klacks:list-attributes source))
         ;; remove XML attributes
         (attributes (remove-if 'xml-attribute-p attributes))
         ;; build an association list
         (attributes (loop
                      for attribute in attributes
                      collecting (cons (attribute-uri attribute source)
                                       (sax:attribute-value attribute))))
         ;; check for duplicate attributes
         (attributes (unique-attributes attributes source))
         ;; check for repeated IDs
         (attributes (ensure-non-repeated-id attributes source)))
    ;; return the attributes
    attributes))

;;;; Language processing

(defun language-tag-p (string)
  "language-tag-p string => boolean
string---a string
language-tag-p return true if the designated string is in the form
specified by RFC-3066. the general form of such a language tag is
<ALPHA>{1,8} ['-' [<ALPHA> | <DIGIT>]{1,8}]*, that is, a string of at
least 1 and at most 8 alphabetic characters followed by any number of
subtags that are a hypen followed by at least 1 and at most 8
alphabetic or digit characters. RFC-3066 also specifies what
particular strings may appear based on country codes, etc., but these
constraints are not enforced here."
  (labels ((alphap (x)
             (or (char<= #\a x #\z)
                 (char<= #\A x #\Z)))
           (digitp (x)
             (char<= #\0 x #\9))
           (primary-subtag (stream)
             (loop for char = (peek-char nil stream nil nil)
                   until (or (null char)
                             (not (alphap char)))
                   counting (read-char stream) into count
                   finally (return (<= 1 count 8))))
           (subtag (stream)
             (loop for char = (peek-char nil stream nil nil)
                   until (or (null char)
                             (not (or (alphap char)
                                      (digitp char))))
                   counting (read-char stream) into count
                   finally (return (<= 1 count 8)))))
    (with-input-from-string (stream string)
      (when (primary-subtag stream)
        (loop for char = (peek-char nil stream nil nil)
              until (null char)
              do (read-char stream)
              always (and (char= char #\-)
                          (subtag stream)))))))
                                
(defvar *current-xml-lang* nil
  "The most recent xml:lang attribute that was specified in the
RDF/XML text. The initial value is nil, and *current-xml-lang* is
always rebound to nil when document parsing begins.")

(defun immediate-xml-lang (source)
  "immediate-xml-lang source => result
source---a cxml:source
result---a string or nil
Immediate-xml-lang returns the value of the xml:lang attribute on the
source. Source's current event should be :start-element. If the
attribute is specified, its value, a string, is returned. If the
attribute is not specified, nil is returned."
  (let ((lang (klacks:get-attribute source "lang" +xml-namespace+)))
    (do () ((or (null lang)
                (string= "" lang)
                (language-tag-p lang))
            lang)
      (restart-case (error 'invalid-language-tag :tag lang :source source)
        (use-value (value)
          :report "Use a different language tag."
          :interactive (lambda ()
                         (list (prompt-for-line "Enter a new language tag:")))
          (setf lang value))
        (ignore-language ()
          :report "Ignore the bad language tag. (Treat as if ~
                   xml:lang attribute had not been provided.)"
          (setf lang nil))))))

(defmacro with-xml-lang (lang &body body)
  "with-xml-lang lang form*
With-xml-lang evalutes lang to generate a new language, and evalutes
body with a new binding of *current-xml-lang*. If the result of
evalating lang is null, then *current-xml-lang* is rebound to its
current value, if it is \"\", then *current-xml-lang* is bound to nil,
otherwise, *current-xml-lang* is bound to the new language."
  (let ((%lang (make-symbol "LANGUAGE")))
    `(let* ((,%lang ,lang)
            (*current-xml-lang*
             (cond ((null ,%lang) *current-xml-lang*)
                   ((string= "" ,%lang) nil)
                   (t  ,%lang))))
       ,@body)))

;;;; li-counters

(defvar *li-counter* '()
  "A list of li-counters. With each expecting-element', a new counter
is pushed onto *li-counter*, and so incrementing the counter of an
element's parent is done by (incf (cadr *li-counter*)).")

(defun expanded-li-uri ()
  "expanded-li-uri => uri
uri---a puri:uri
Expanded-li-uri returns the uri generated by incrementing the current
element's parent's li counter and adding the fragment _n (where n is
the incremented counter) to the RDF namespace."
  (let ((n (incf (cadr *li-counter*))))
    (add-fragment (strcat #\_ (write-to-string n)) +rdf-namespace+)))

;;;; Expecting Element

(defun check-for-illegal-namespace-names (attributes)
  "check-for-illegal-namespace-names attributes => |
attributes---a list of attributes
check-for-illegal-namespace-names enforces the restriction of Section
5.1 of the RDF/XML Syntax Specification that states that \"within
RDF/XML documents it is not permitted to use XML namespaces whose
namespace name is the RDF namespace URI reference concatenated with
additional characters.\" If such a namespace binding is encountered,
an error of type illegal-namespace-name is signalled."
  (dolist (namespace-declaration
           (cxml::find-namespace-declarations attributes))
    (destructuring-bind (prefix . uri) namespace-declaration
      (multiple-value-bind (prefixp properp) (prefixp +rdf-namespace+ uri)
        (when (and prefixp properp)
          (error 'illegal-namespace-name :prefix prefix :uri uri))))))

(defmacro expecting-element
          ((source &optional (lname nil lnamep) (uri nil urip)) &body body)
  "expecting-element (source [lname [uri]]) form*
expecting-element is a wrapper around klacks:expecting element that
ensures proper binding of the *current-xml-lang* variable, so that
plain literals can properly inherit the value of xml:lang attributes.
Within this RDF/XML parser, expecting-element should always be used
rather than klacks:expecting-element."
  (let ((%source (make-symbol "SOURCE")))
    `(let ((,%source ,source))
       (klacks:expecting-element (,%source
                                  ,@(when lnamep `(,lname))
                                  ,@(when urip `(,uri)))
         ;; TODO: The attributes are already being extracted here, so
         ;; it might be worthwhile to process them all here and
         ;; somehow cache them (and partition them based on how
         ;; they're used) so that we don't extract more times than
         ;; necessary.
         (check-for-illegal-namespace-names
          (klacks:list-attributes ,%source))
         (with-xml-lang (immediate-xml-lang ,%source)
           (let ((*li-counter* (cons 0 *li-counter*)))
             ,@body))))))

;;;; Conditions

(defun current-position (source)
  "current-position source => position
source---a cxml:source
position---a string
Current-position returns a string of the form <line>.<column>
indicating the approximate position of source."
  (format nil "~s.~s"
          (klacks:current-line-number source)
          (klacks:current-column-number source)))

(define-condition rdfxml-warning (warning) ()
  (:documentation
   "The class of warnings signalled by the RDF/XML parser."))

(define-condition rdf-prefixed-non-rdf-name (rdfxml-warning)
  ((name :initarg :name :reader name))
  (:documentation "According to \"Section 5.1 The RDF Namespace and
Vocabulary\" of the RDF/XML Syntax Specification, certain names are
defined as RDF names, and these begin with the RDF namespace name, but
\"any other names [beginning with the RDF namespace name] are not
defined and SHOULD generate a warning when encountered, but should
otherwise behave normally.\" rdf-prefixed-non-rdf-name is the class of
warnings that are signalled in such situations.")
  (:report
   (lambda (condition stream)
     (format stream "The name ~A was encountered, which begins with ~
                     the RDF namespace name, but is not a defined ~
                     RDF name. (See Section 5.1 of the RDF/XML ~
                     Syntax Specification for more information."
             (name condition)))))

(defvar *warn-on-rdf-prefixed-non-rdf-names* t
  "According to to Section 5.1, The RDF Namespace and Vocabulary of
the RDF/XML Syntax Specification, warnings SHOULD be generated when a
name is encountered that begins with the RDF namespace name, but is
not an RDF name. If *warn-on-rdf-prefixed-non-rdf-names* is true (the
default), then such warnings are generated, but are muffled otherwise.")

(define-condition non-namespaced-name (rdfxml-warning)
  ((name :initarg :name :reader name))
  (:documentation "According to 6.1.4 of the RDF/XML Syntax
Specification, the attributes ID, about, resource, parseType, and type
may appear without a namespace prefix, and are interpreted as the
corresponding RDF names. Also, \"new documents SHOULD NOT use these
unqualified attributes, and applications MAY choose to warn when the
unqualified form is seen in a document.\" non-namespaced-name is the
class of warnings that are signalled in such situations.")
  (:report
   (lambda (condition stream)
     (format stream "Unqualified attribute ~A is being placed into ~
                     the RDF namespace according to Section 6.1.4 of ~
                     the RDF/XML Syntax Specification, but new ~
                     documents should not use unqualified attribues."
             (name condition)))))

(defvar *warn-on-non-namespaced-names* t
  "A boolean (whose default value is true) that controls whether a
warning is signalled when a permitted non-namespaced attribute is
encountered. The only attributes which may appear without namespaces
are ID, about, resource, parseType, and type. New documents should not
use unqualified forms, though they may appear in legacy documents. See
Section 6.1.4 of the RDF/XML Syntax Specification.")

(define-condition other-parse-type (rdfxml-warning)
  ((parse-type :initarg :parse-type :reader parse-type))
  (:documentation "The rdf:parseType attribute has three explicitly
meaning values, \"Resource\", \"Literal\", and \"Collection\". If
rdf:parseType is encountered with a different value, the element is
processed as though the value had been \"Literal\". The specification
does not indicate that a warning should be signalled, and so such
warnings are not generated in the default case, but if the user
requests warnings on such attribute values, a warning of type
other-parse-type is signalled.")
  (:report
   (lambda (condition stream)
     (format stream "The rdf:parseType attribute appeared with value ~
                     ~S, which is not one of \"Literal\", ~
                     \"Resource\", or \"Collection\".  The element ~
                     will be processed as though the value was ~
                     \"Literal\"." (parse-type condition)))))

(defvar *warn-on-parse-type-other* nil
  "A boolean (whose default value is false) that controls whether a
warning is signalled when an element is encountered that specifies the
rdf:parseType attribute with a value other than \"Literal\",
\"Resource\", or \"Collection\". Such an element is treated as though
the value were \"Literal\", and this situation is not an error.
Nonetheless, it seems likely that one might be interested in knowing
when it occurs.")

;;; Errors

(define-condition rdfxml-error (error)
  ((source :initarg :source :reader source))
  (:documentation "The class of errors signalled by the RDF/XML parser."))

;;; Illegal namespace name

(define-condition illegal-namespace-name (rdfxml-error)
  ((prefix :initarg :prefix :reader prefix)
   (uri    :initarg :uri    :reader uri))
  (:documentation "According to Section 5.1 of the RDF/XML Syntax
Specification, Within RDF/XML documents it is not permitted to use XML
namespaces whose namespace name is the RDF namespace URI reference
concatenated with additional characters.")
  (:report
   (lambda (condition stream)
     (format stream "Within RDF/XML documents it is not permitted to ~
                     use XML namespaces whose namespace name is the ~
                     RDF namespace URI reference concatenated with ~
                     additional characters, but the prefix ~S would ~
                     have been bound to the name ~S."
             (prefix condition)
             (uri condition)))))

;;; Bad attribute value

(define-condition invalid-attribute-value (rdfxml-error)
  ((value     :initarg :value     :reader value)
   (attribute :initarg :attribute :reader attribute))
  (:documentation "Conditions of type invalid-attribute-value are
signalled when an attribute value is not appropriate for the
particular attribute. This kind of situation may happen, for instance,
if a xml:lang value is not RFC 3066 compliant, or if an rdf:ID or
rdf:nodeID value is not an XML NCName. Note that these situations are
distinct from those in which an attribute appears where it should not."))

(define-condition repeated-id (invalid-attribute-value) ()
  (:documentation "Errors of type repeated-id are signalled when the
value of an rdf:ID on an element is the same as the value of an rdf:ID
attribute on another element. rdf:IDs should be unique within the a
document.")
  (:report
   (lambda (condition stream)
     (format stream "Near ~A, rdf:ID appeared with value ~S, but ~
                     an element has already appeared with this same ID."
             (current-position (source condition))
             (value condition)))))

(define-condition non-nc-name-id (invalid-attribute-value) ()
  (:documentation "Errors of type non-nc-name-id are raised when
attributes rdf:ID or rdf:nodeID appear with values that are not valid
NCNames.")
  (:report
   (lambda (condition stream)
     (format stream "Near ~A attribute ~S appeared with value ~S ~
                     which should be, but is not, an XML NCName."
             (current-position (source condition))
             (attribute condition)
             (value condition)))))

(define-condition invalid-language-tag (invalid-attribute-value)
  ((tag :initarg :tag :reader tag))
  (:documentation "Language tags in RDF/XML (and more generally, XML)
must be in accordance with RFC 3066. When a language tag is specified
that is not of the proper form, an error of type invalid-language-tag
is signalled.")
  (:report
   (lambda (condition stream)
     (format stream "~S is not an RFC 3066 compliant language tag, ~
                     but was encountered near ~a."
             (tag condition)
             (current-position (source condition))))))

;;; Unexpected characters

(define-condition unexpected-characters (rdfxml-error)
  ((characters :initarg :characters :reader characters))
  (:documentation "Excess whitespace is always permitted between
elements, but arbitrary character data is not. When non-whitespace
character data is encountered where whitespace is expected, an error
of type unexpected characters is signalled.")
  (:report
   (lambda (condition stream)
     (format stream "The text ~s appeared near ~a, but only whitespace
                     is permitted."
             (characters condition)
             (current-position (source condition))))))

;;; Duplicate attributes

(define-condition duplicate-attribute (rdfxml-error)
  ((attribute :initarg :attribute :reader attribute))
  (:documentation "Errors of type duplicate-attribute are signalled
when attributes are specified more than once and the XML parser did
not flag the error. This happens when, according to the RDF/XML
specification, certain non-namespaced attributes are interpreted as
being in the RDF namespace. A duplicate attribute can appear, for
instance, when rdf:ID is specified in conjunction with ID, which is
interpreted as rdf:ID.")
  (:report
   (lambda (condition stream)
     (format stream "Duplicate attribute ~s appeared near ~a."
             (attribute condition)
             (current-position (source condition))))))

;;; Prohibited attributes

(define-condition prohibited-attribute (rdfxml-error)
  ((attribute :initarg :attribute :reader attribute))
  (:documentation "At various places in RDF/XML, the set of attributes
permissible on an element is restricted. When an attribute appears on
an element but is not allowed there, an prohibited-attribute error is
signalled.")
  (:report
   (lambda (condition stream)
     (format stream "Attribute ~s, appearing near ~a, is not ~
                     permitted in this context."
             (attribute condition)
             (current-position (source condition))))))

(define-condition datatyped-empty-property (prohibited-attribute) ()
  (:documentation "Errors of type datatyped-empty-property are
signaled when the parser is attempting to parser an empty property,
but an rdf:datatype attribute is specified. rdf:datatype is not a
permissible attribute on on empty properties, but it is expected that
this case will most likely arise when the intent was to generate a
literal element with an empty string. When this type of error is
signalled, it is expected that one of the available restarts will make
an attempt to parse the element as a literal with a null string. This
issue is also in the errata of the RDF/XML syntax specification.")
  (:report
   (lambda (condition stream)
     (format stream "Datatype attribute ~s, appearing near ~a, is ~
                     not permitted on empty property elements."
             (attribute condition)
             (current-position (source condition))))))

(defvar *coerce-datatyped-empty-properties* nil
  "A boolean (whose default value is false) that controls whether an
empty property element that contains a datatype attribute (a case
which is prohibited by the RDF/XML Syntax Specification) is coerced to
a typed literal with the specified datatype and the empty string \"\"
as a lexical form. Also see the condition type
datatyped-empty-property and the keyword argument
coerce-datatyped-empty-properties to the function parse-document.")

(define-condition non-namespaced-attribute (prohibited-attribute) ()
  (:documentation "Certain attribute, namely rdf:ID, rdf:about,
rdf:resource, rdf:parseType, and rdf:type are permitted to appear
without a namespace specified. These attributes are automatically
treated as though they had appeared with the RDF namespace prefix. Any
other attributes without namespaces, however, must not appear.")
  (:report
   (lambda (condition stream)
     (format stream "Only ID, about, resource, parseType, and type may ~
                     appear without namespaces, but ~S appeared as ~
                     a local name without a namespace."
             (attribute condition)))))

;;; Mutually exclusive attributes
             
(define-condition mutually-exclusive-attributes (rdfxml-error)
  ((attributes :initarg :attributes :reader attributes))
  (:documentation "Some elements are permitted to contain one of a set
of attributes, but no more than one of the set. That is, there are
attributes that are permitted on an element, but are mutually
exclusive. This class of error is signalled when such attributes are
encountered.")
  (:report
   (lambda (condition stream)
     (format stream "The attributes ~s were provided near ~a, ~
                     but are mutually exclusive in this context"
             (mapcar 'car (attributes condition))
             (current-position (source condition))))))

;;; Bad URI classes

(define-condition bad-uri (rdfxml-error)
  ((uri :initarg :uri :reader uri))
  (:documentation "Errors that are subclasses of bad-uri are signalled
when a URI appears in a place that a URI is required, but the provided
URI is not a valid URI for that place."))

(define-condition bad-property-element-uri (bad-uri) ()
  (:documentation "Errors of type bad-property-element-uri are
signalled when a URI that is not a property-element-uri is specified
in a position where a property-element-uri is expected.")
  (:report
   (lambda (condition stream)
     (format stream "The URI ~s that appeared near ~a is not a valid ~
                     property-element URI."
             (uri condition)
             (current-position (source condition))))))

(define-condition bad-node-element-uri (bad-uri) ()
  (:documentation "Errors of type bad-node-element-uri are signalled
when a URI that is not a node-element-uri is specified in a position
where a node-element-uri is expected.")
  (:report
   (lambda (condition stream)
     (format stream "The URI ~s that appeared near ~a is not a valid ~
                     node-element URI."
             (uri condition)
             (current-position (source condition))))))

;;;; Restarts

(defun ignore-language (&optional condition)
  "ignore-language [condition] => |
condition---a condition
Ignore-language treats a xml:lang attribute whose value was not a
language tag conforming to RFC 3066 as though the attribute had not
been specified. This occurs by invoking the restart ignore-language."
  (declare (ignore condition))
  (invoke-restart 'ignore-language))

(defun ignore-attribute (&optional condition)
  "ignore-attribute [condition] => |
condition---a condition
Ignore-attribute attempts to invoke the restart named
ignore-attribute. This is intended for use when an attribute appears
in a place where it is prohibited, but parsing would continue
successfully if the attribute had not been specified."
  (declare (ignore condition))
  (invoke-restart 'ignore-attribute))

(defun ignore-attributes (&optional condition)
  "ignore-attributes [condition] => |
condition---a condition
Ignore-attributes attempts to invoke the restart named
ignore-attributes. This is intended for use when duplicate attributes
are provided and all can be ignored, or when mutually exclusive
attributes appear, and all can be ignored."
  (declare (ignore condition))
  (invoke-restart 'ignore-attributes))

(defun ignore-characters (&optional condition)
  "ignore-characters [condition] => |
condition---a condition
Ignore-characters attempts to invoke the restart named
ignore-characters. This is intended for use when character data
appears in a place that should have been whitespace."
  (declare (ignore condition))
  (invoke-restart 'ignore-characters))

(defun parse-as-typed-literal (&optional condition)
  "parsed-as-typed-literal [condition] => |
condition---a condition
parse-as-typed-literal attempts to parse an empty-property-element as
typed literal. This is intended to be used when an rdf:datatype
attribute is present on an empty property element. Strictly speaking,
this is prohibited by the RDF/XML specification (see errata), but some
RDF/XML parses output it anyway."
  (invoke-restart (find-restart 'parse-as-typed-literal condition)))

(defun parse-uri-non-strictly (condition)
  "parse-uri-non-strictly condition => result*
condition---a condition
Parse-uri-non-strictly is a restart function that invokes the restart
named parse-uri-non-strictly. If no such restart is available,
parse-uri-non-strictly returns NIL, otherwise the results of invoking
the restart are returned."
  (let ((restart (find-restart 'parse-uri-non-strictly condition)))
    (unless (null restart)
      (invoke-restart restart))))

(defvar *parse-uris-strictly* t
  "A boolean (whose default value is true) that controls whether URIs
are parsed strictly. This determines whether, within parse-document, a
handler is established for conditions of type puri:uri-parse-error
which will attempt to parse malformed URIs in a non-strict way. Also
see the restart function parse-uri-non-strictly, and the
parse-uris-strictly keyword argument to parse-document.")

;;; Conveniently establishing restarts

(defmacro with-alist-restarts ((thing alist) restartable-form &body body)
  "with-alist-restarts (thing alist) form &body body
thing---a symbol
alist---an association list
restartable-form---a form
body---forms
With-alist-restarts evaluates restartable form within a dynamic
context in which a restart is established for each entry in alist. If
the restart corresponding to an entry is invoked, then thing is bound
to the entry's cons cell, and body is evaluated, and the result of
body is the result of the form. If restartable-form does not signal an
error, then the result is the result of restartable form."
  (let ((get-value (make-symbol "GET-VALUE"))
        (fun (make-symbol "ESTABLISH-RESTARTS"))
        (items (make-symbol "ITEMS"))
        (stream (make-symbol "STREAM")))
    `(block ,get-value 
       (labels ((,fun (,items)
                  (cond
                   ((endp ,items) ,restartable-form)
                   (t (restart-bind
                          ((nil (lambda ()
                                  (return-from ,get-value
                                    ((lambda (,thing) ,@body)
                                     (car ,items))))
                                :report-function
                                (lambda (,stream)
                                  (format ,stream "Use ~S = ~S."
                                          (caar ,items)
                                          (cdar ,items)))))
                        (,fun (cdr ,items)))))))
         (,fun ,alist)))))

;;;; Parsing RDF and emitting triples

;;; Triple Emission

(defvar *subject* nil
  "The subject of the triples being processed.")

(defvar *predicate* nil
  "The predicate of the triples being processed.")

(defvar *object* nil
  "The predicate of the triples being processed.")

(defvar *triple-receiver* nil
  "A function that receives the triples.")

(defun emit-triple
       (&key (subject *subject*) (predicate *predicate*) (object *object*))
  "emit-triple &key subject predicate object => value*
subject, predicate, object---objects
Emit-triple calls the value of *triple-receiver* with the three values
subject, predicate, and object. subject, predicate, and object, if not
provided, default, respecively, to *subject*, *predicate*, and
*object*."
  (funcall *triple-receiver* subject predicate object))

(defun reify-triple (triple-uri)
  "reify-triple triple-uri => |
triple-uri---a PURI URI
Reify-triple emits the four triples that reify the triple <*subject*
*predicate* *object*>. Triple-uri is the URI of the reified triple."
  (emit-triple :subject triple-uri
               :predicate +rdf-subject+
               :object *subject*)
  (emit-triple :subject triple-uri
               :predicate +rdf-predicate+
               :object *predicate*)
  (emit-triple :subject triple-uri
               :predicate +rdf-object+
               :object *object*)
  (emit-triple :subject triple-uri
               :predicate +rdf-type+
               :object +rdf-statement+))

;;; Document Parsing

(defun parse-document
       (function input
                 &key
                 ((:coerce-datatyped-empty-properties
                   *coerce-datatyped-empty-properties*)
                  *coerce-datatyped-empty-properties*)
                 ((:parse-uris-strictly
                   *parse-uris-strictly*)
                  *parse-uris-strictly*)
                 ((:warn-on-rdf-prefixed-non-rdf-names
                   *warn-on-rdf-prefixed-non-rdf-names*)
                  *warn-on-rdf-prefixed-non-rdf-names*)
                 ((:warn-on-non-namespaced-names
                   *warn-on-non-namespaced-names*)
                  *warn-on-non-namespaced-names*)
                 ((:warn-on-parse-type-other
                   *warn-on-parse-type-other*)
                  *warn-on-parse-type-other*))
  "parse-document function input => |
function---a function of three arguments
input---an input suitable for cxml:make-source
Parse-document parses input, binds *triple-receiver* to function, and
calls emit-triple with each triple that can be extracted from the
input. *blank-nodes* is rebound to a new equal hash table and maps
blank node identifiers to their blanks nodes. If the document element
is rdf:RDF, then its children are processed as node elements,
otherwise, the body of the document is parsed as a sequence of node
elements."
  (let ((*element-ids* (make-hash-table :test 'equal))
        (*blank-nodes* (make-hash-table :test 'equal))
        (*triple-receiver* (coerce function 'function))
        (source (cxml:make-source input)))
    (handler-bind ((datatyped-empty-property
                    (lambda (condition)
                      (when *coerce-datatyped-empty-properties*
                        (parse-as-typed-literal condition))))
                   (puri:uri-parse-error
                    (lambda (condition)
                      (unless (or *parse-uris-strictly*
                                  (not puri:*strict-parse*))
                        (parse-uri-non-strictly condition))))
                   (rdf-prefixed-non-rdf-name
                    (lambda (condition)
                      (unless *warn-on-rdf-prefixed-non-rdf-names*
                        (muffle-warning condition))))
                   (non-namespaced-name
                    (lambda (condition)
                      (unless *warn-on-non-namespaced-names*
                        (muffle-warning condition))))
                   (other-parse-type
                    (lambda (condition)
                      (unless *warn-on-parse-type-other*
                        (muffle-warning condition)))))
      (klacks:with-open-source (in source)
        (klacks:skip in :start-document)
        (consume-whitespace source)
        (when (eq (klacks:peek in) :start-element)
          (if (not (eq (element-uri in) +rdf-rdf+))
            (node-element-list in)
            (expecting-element (in)
              ;; no attributes are allowed here, but if one should
              ;; appear, the option is available to ignore it.
              (doalist ((uri value) (element-attributes in))
                (declare (ignore value))
                (restart-case (error 'prohibited-attribute
                                     :attribute uri :source source)
                  (ignore-attribute ()
                    :report "Ignore the attribute.")))
              (klacks:skip in :start-element)
              (node-element-list in))))))))

;;; Node Elements

(defun node-element-list (source &key (collect nil))
  "node-element-list source &key collect => [nodes]
source---a cxml-source
Node-element-list processes a node element list, corresponding to
production 7.2.10, ws* (nodeElement ws*)*. If collect is non-nil, then
the nodes are collected and returned."
  (loop
   initially (consume-whitespace source)
   while (eq :start-element (klacks:peek source))
   for node = (node-element source)
   when collect collect node 
   do (consume-whitespace source)))

(defun node-element-components (source)
  "node-element-components source => property-attributes, this
source---a cxml:source
property-attributes---an alist
this---a blank node or a URI
Node-element-components returns an alist of the property-attributes on
the element at which source should be positioned, and a blank node or
URI indicating the object that this node element represents. At most
one of rdf:ID, rdf:nodeID, and rdf:about should be specified, and an
error is signalled if more than one is specified. An error is also
signalled if any the URI of any attribute is not a valid
property-attribute URI."
  (let ((identifiers '())
        (property-attributes '())
        (attributes (element-attributes source)))
    ;; rdf:nodeID, rdf:ID, and rdf:about identify the node element
    ;; object, and are saved in identifiers. Attributes whose URIs are
    ;; property-attribute-uris are saves in property-attributes. Other
    ;; attributes are prohibited.
    (doalist ((uri value pair) attributes)
      (declare (ignore value))
      (cond
       ((or (eq uri +rdf-id+)
            (eq uri +rdf-node-id+)
            (eq uri +rdf-about+))
        (push pair identifiers))
       ((property-attribute-uri-p uri)
        (push pair property-attributes))
       (t (restart-case (error 'prohibited-attribute
                               :attribute uri
                               :source source)
            (ignore-attribute ()
              :report "Ignore the attribute.")))))
    ;; rdf:ID, rdf:nodeID, and rdf:about are mutually exclusive, so if
    ;; more than one is specified, we can ignore them all, or choose
    ;; one of those that was provided.
    (do () ((endp (rest identifiers)))
      (with-alist-restarts (id identifiers)
          (restart-case (error 'mutually-exclusive-attributes
                               :attributes identifiers
                               :source source)
            (ignore-attributes ()
              :report "Ignore all the offending attributes ~
                       (Object will become a blank node)."
              (setf identifiers '())))
        (setf identifiers (list id))))
    ;; Afterward, the property-attributes are returned as the primary
    ;; value, and the indicator for the element as the secondary.
    (values
     property-attributes
     (node-element-subject (first identifiers) source))))

(defun node-element-subject (this source)
  "node-element-subject this base => result
this---a cons, or nil
source---a cxml:source
Node-element-subject returns the object designated by this (the second
return value of node-element-attributes) and base. If this is null,
then a blank-node is returned. Otherwise, the car of this should be
either +rdf-node-id+, +rdf-about+, or +rdf-id+, and result is
determined as follows: If the uri is +rdf-node-id+, then a blank node
with an id is returned, where the id is the cdr of this. If the uri is
+rdf-about+, then the cdr of the this is mergedd with base. If uri is
+rdf-id+, then the cdr of this is added as a fragment to base."
  (let ((base (klacks:current-xml-base source)))
    (if (null this) (blank-node)
      (destructuring-bind (uri . value) this
        (cond
         ((eq uri +rdf-about+)
          (merge-uris value base))
         ;; If the attribute is rdf:ID or rdf:nodeID, then we must
         ;; ensure that the attribute value is a valid name, that is, is
         ;; an XML NCName. We can restart by changing the value, making
         ;; a blank node, or using the value anyway.
         ((or (eq uri +rdf-node-id+)
              (eq uri +rdf-id+))
          (flet ((final-value (value)
                   (if (eq uri +rdf-node-id+)
                     (blank-node value)
                     (add-fragment value base))))
            (do () ((id-name-p value) (final-value value))
              (restart-case (error 'non-nc-name-id
                                   :source source
                                   :attribute +rdf-node-id+
                                   :value value)
                (ignore-attribute ()
                  :report "Ignore the attribute and make an anonymous blank node."
                  (return (blank-node)))
                (use-it ()
                  :report "Use it anyway."
                  (return (final-value value)))
                (use-value (new-value)
                  :report "Use a different value."
                  :interactive (lambda ()
                                 (list (prompt-for-line "Enter a new value:")))
                  (setf value new-value))))))
         ;; This error isn't restartable, because user code should never
         ;; call this function. If this error is signalled, something
         ;; has gone wrong that the parser author needs to address.
         (t (error "In determining the subject corresponding to a node, ~
                  the designator ought to be nil or one of the ~
                  following attributes: rdf:nodeID, rdf:about, ~
                  rdf:ID, but the attribute was ~s." (car this))))))))

(defun node-element (source)
  "node-element source => node
source---a cxml source
Node-element processes the source's current node. Source should be at
a :start-element, and the element should correspond to a node. Triples
represented in the node will be emitted. The node is returned."
  (expecting-element (source)
    (let ((uri (element-uri source))
          (base (klacks:current-xml-base source)))
      ;; ensure that uri is a valid element-uri
      (do () ((node-element-uri-p uri))
        (restart-case (error 'bad-node-element-uri
                             :uri uri
                             :source source)
          (use-value (value okp)
            :report "Specify a URI to use."
            :interactive prompt-for-uri
            (when okp (setf uri (merge-uris value base))))))
      ;; and once it is...
      (multiple-value-bind (attributes *subject*)
          (node-element-components source)
        (klacks:skip source :start-element)
        (prog1 *subject*
          ;; When the element-uri is not rdf:Description, it specifies
          ;; the type of the object denoted by the element, and a type
          ;; triple is emitted.
          (unless (eq uri +rdf-description+)
            (emit-triple :predicate +rdf-type+ :object uri))
          ;; handle the other attributes. This is just processing the
          ;; attribute association list and emitting triples with the
          ;; current subject, with the key as the predicate and the
          ;; value as the object as a plain-literal. There is one
          ;; exception: when the attribute is +rdf-type+, the value is
          ;; interpreted as a URI reference.
          (doalist ((uri value) attributes)
            (if (eq uri +rdf-type+)
              (emit-triple
               :predicate +rdf-type+
               :object (intern-uri value))
              (emit-triple
               :predicate uri
               :object (intern-plain-literal value *current-xml-lang*))))
          ;; The children of the node element are processed as
          ;; property-elements, and get processed in document order.
          (property-element-list source))))))

;;; Property Elements

(defun property-element-list (source)
  "property-element-list source => |
source---a cxml source
Property-element-list processes a property element list, corresponding
to production 7.2.13, ws* (propertyElt ws*)*."
  (loop
   initially (consume-whitespace source)
   while (eq :start-element (klacks:peek source))
   do (property-element source)
   do (consume-whitespace source)))

(defun property-element (source)
  "property-element source => |
source---a cxml source
Property-element processes the current property element, according to
production 7.2.14. There are actually a number of different kinds of
property elements that can appear, and so process-property element
processes enough of the element to determine which type, and then
dispatches to more specific property element processing functions."
  ;; Expecting-element is used here, so any functions called from here
  ;; must not consume the corresponding :end-element event. If the
  ;; element-uri is rdf:li, then it's rewritten with the corresponding
  ;; expanded form, which is rdf:_n where n is the incremented
  ;; li-counter of the parent element.
  (expecting-element (source)
    (let* ((uri (element-uri source))
           (*predicate* (if (eq uri +rdf-li+) (expanded-li-uri) uri))
           (attributes (element-attributes source))
           (parse-type (cdr (assoc +rdf-parse-type+ attributes))))
      ;; Make sure that *predicate* is a property element URI.
      (do () ((property-element-uri-p *predicate*)) 
        (restart-case (error 'bad-property-element-uri
                             :uri *predicate*
                             :source source)
          (use-value (value okp)
            :report "Supply a different URI to use."
            :interactive prompt-for-uri
            (when okp (setf *predicate* value)))))
      ;; If there is no rdf:parseType attribute, then the the
      ;; property element is handled by process-non-parse-
      ;; property-element, otherwise the attribute value
      ;; determines how the property element is parsed, if the
      ;; value is Literal, Resource, or Collection.  Any other
      ;; value causes it to be parsed in the same way as Literal.
      (cond
       ((null parse-type)
        (non-parse-type-property-element source))
       ((string= "Resource" parse-type)
        (parse-type-resource-property-element source))
       ((string= "Collection" parse-type)
        (parse-type-collection-property-element source))
       ((string= "Literal" parse-type)
        (parse-type-literal-property-element source))
       ;; It might not be necessary to warn here--the behaviors for
       ;; values other than Collection, Literal and Resource are well
       ;; defined
       (t (warn 'other-parse-type :parse-type parse-type)
          (parse-type-literal-property-element source))))))

(defun parse-type-property-element-id (source &aux (id nil))
  "parse-type-property-element-id source => id
source---a cxml:source
id---a URI or nil
Elements that specify the rdf:parseType attribute may only have one
other attribute, rdf:ID. This function extracts the value of the
rdf:ID attribute if specified and returns a URI which is the base uri
with the value of the ID attribute as a fragment id or nil if there is
no such attribute. An error is signalled if rdf:ID is specified more
than once, or if an attribute other than rdf:parseType or rdf:ID is
specified."
  (let ((base (klacks:current-xml-base source)))
    (doalist ((uri value) (element-attributes source) id)
      (cond
       ;; values of rdf:ID attributes must be valid names. If they
       ;; aren't, then we could restart by using it anyway, ignoring
       ;; it, or specifying a new value.
       ((eq uri +rdf-id+)
        (do () ((id-name-p value) (setf id (add-fragment value base)))
          (restart-case (error 'non-nc-name-id
                               :attribute +rdf-id+
                               :value value
                               :source source)
            (nil ()
              :report "Use it anyway."
              (return (setf id (add-fragment value base))))
            (ignore-attribute ()
              :report "Ignore the attribute. (No reification will occur.)"
              (return (setf id nil)))
            (use-value (new-value okp)
              :report "Use a different value."
              :interactive (lambda ()
                             (multiple-value-list
                              (prompt-for-line "Enter a new ID:")))
              (when okp (setf value new-value))))))
       ;; otherwise, rdf:parseType is silently ignored (it's
       ;; permitted, but we don't need its value right now), and other
       ;; attribute are prohobited, but can be ignored.
       ((eq uri +rdf-parse-type+))
       (t (restart-case (error 'prohibited-attribute
                               :attribute uri
                               :source source)
            (ignore-attribute ()
              :report "Ignore the attribute.")))))))

(defun element-literal-content (source &key (consume-end t))
  "element-literal-content source &key consume-end => content
source---a cxml:source
content---a string
consume-end---a boolean
Element-literal-content returns a string that is the literal content
of the of element at source. The current event of source should be
:start-element of the element whose literal content is to be
extracted. The corresponding :end-element is consumed if consume-end
is non-nil (the default)."
  (let ((sink (cxml:make-rod-sink :canonical 1)))
    (cxml:with-xml-output sink
      (klacks:skip source :start-element)
      (loop (let ((event (klacks:peek source)))
              (case event
                (:start-element
                 (klacks:serialize-element
                  source sink :document-events nil))
                (:end-element
                 (when consume-end
                   (klacks:consume source))
                 (return))
                (t (klacks:serialize-event
                    source sink :consume t))))))))

(defun parse-type-literal-property-element (source)
  "parse-type-literal-property-element source => |
source---a cxml source
Parse-type-literal-property-element binds *object* to a typed literal
whose type is rdf:XMLLiteral, and whose content is the text content of
the current element. If the current element has an rdf:ID attribute,
then the triple is reified with the ID."
  (let* ((id (parse-type-property-element-id source))
         (xml-content (element-literal-content source :consume-end nil))
         (*object* (intern-typed-literal xml-content +rdf-xml-literal+)))
    (emit-triple)
    (when id (reify-triple id))))

(defun parse-type-resource-property-element (source)
  "parse-type-resource-property-element source => |
source---a cxml source
parse-type-resource-property-element processes a property element with
parseType Resource, according to production 7.2.18."
  ;; When an element is parsed as a resource, a new blank node is
  ;; generated for it, *object* is bound to the blank node, and a
  ;; triple is emitted. If rdf:ID is specified, then the triple is
  ;; also reified with the specified ID (merged with the current
  ;; base). Then *subject* is bound to the newly generated blank node
  ;; and the children of the element are processed.
  (let* ((*object* (blank-node))
         (id (parse-type-property-element-id source)))
    (klacks:skip source :start-element)
    (emit-triple)
    (when id (reify-triple id))
    (let ((*subject* *object*))
      (property-element-list source))))

(defun parse-type-collection-property-element (source)
  "parse-type-collection-property-element source => |
source---a cxml source
parse-type-collection-property-element processes the content of a
property element that has parseType=\"Collection\".  Triples are
emitted that describe the sequence of nodes within the property
element."
  ;; First, *predicate* is bound to the element-uri of the current
  ;; element, and the rdf:ID (if available) is extracted. These are
  ;; extracted first so that :start-element may be skipped, and the
  ;; children nodes processed. One blank node is generated for each of
  ;; the children (the rdf:first of each blank node will be the
  ;; corresponding child). Then *object is bound to the head of the
  ;; list of blank nodes, and the rdf:first/rdf:rest for each blank
  ;; node (like a Lisp cons cell) is emitted.
  (let ((*predicate* (element-uri source))
        (id (parse-type-property-element-id source)))
    (klacks:skip source :start-element)
    (let* ((nodes (node-element-list source :collect t))
           (bnodes (loop repeat (length nodes) collect (blank-node)))
           (*object* (if (endp bnodes) +rdf-nil+ (first bnodes))))
      (emit-triple)
      (when id (reify-triple id))
      (loop for item in nodes
            for (cons cdr) on bnodes
            for rest = (or cdr +rdf-nil+)
            do (emit-triple :subject cons
                            :predicate +rdf-first+
                            :object item)
            do (emit-triple :subject cons
                            :predicate +rdf-rest+
                            :object rest)))))

(defun non-parse-type-property-element (source)
  "non-parse-type-property-element source => |
source---a cxml:source
If a property element does not specify the rdf:parseType attribute,
then the property-element is parsed with either
resource-property-element, literal-property-element, or
empty-property-element. Resource-property-element matches in the case
that there is a single element within the property-element,
literal-property-element matches when there is only text content
within the property-element, and empty-property-element matches when
there is no content whatsoever within the property-element."
  (let ((strings '())
        (attributes (element-attributes source)))
    ;; Collect all the text following the start-element
    (loop initially (klacks:skip source :start-element)
          while (eq :characters (peek-skipping-comments source))
          collect (klacks:peek-value source) into ^strings
          do (klacks:consume source)
          finally (setf strings ^strings))
    (ecase (klacks:peek source)
      ;; If the next event is :start-element, then the
      ;; property-element ought to be a resource-property-element,
      ;; matching ws* NodeElement ws* which means that all the text
      ;; consumed should have been whitespace.
      ((:start-element)
       (dolist (string strings)
         (unless (xml-whitespace-p string)
           (restart-case (error 'unexpected-characters
                                :characters string :source source)
             (ignore-characters ()
               :report "Ignore the characters."))))
       (resource-property-element source attributes)
       (consume-whitespace source))
      ;; If an end element, then the property-element is either a
      ;; literal-property-element, and the combined text is the value,
      ;; or there was no character data, and the property-element is
      ;; an empty-property-element. If the property-element is an
      ;; empty-property-element, then we can handle it here by trying
      ;; to parse as literal property-element with the null string.
      ((:end-element)
       (if (endp strings)
         (restart-case (empty-property-element source attributes)
           (parse-as-typed-literal ()
             :report "Try parsing the element as a typed literal ~
                      with null string."
             :test (lambda (condition)
                     (typep  condition 'datatyped-empty-property))
             (literal-property-element source attributes "")))
         (literal-property-element
          source attributes
          (with-output-to-string (*standard-output*)
            (dolist (s strings)
              (write-string s)))))))))

(defun resource-property-element (source attributes)
  "resource-property-element source attributes => |
source---a cxml:source
attributes---an alist
resource-property-element finished the processing of a resource
property element that should have begun with
non-parse-type-property-element. source should be positioned at the
:start-element of the node element within the property-element.
resource-property-element does not consume trailing whitespace after
the node element. The only attribute permitted on the resource
property-element is rdf:ID, and an error is signalled if other
attributes are specified, or rdf:ID is specified more than once."
  ;; Check that only rdf:ID is specified...
  (doalist ((uri value) attributes)
    (declare (ignore value))
    (unless (eq uri +rdf-id+)
      (restart-case (error 'prohibited-attribute
                           :attribute uri :source source)
        (ignore-attribute ()
          :report "Ignore the attribute"))))
  ;; ...and then process the property-element.
  (let ((id (cdr (assoc +rdf-id+ attributes)))
        (*object* (node-element source)))
    (emit-triple)
    (when id
      (reify-triple (add-fragment id (klacks:current-xml-base source))))))

(defun literal-property-element (source attributes text)
  "literal-property-element source attributes text => |
source---a cxml:source
attributes---an alist
text---a string
Literal-property-element emits the triple corresponding to the literal
property element. Text is the literal text of the literal property.
Attributes is the alist of attributes of the property-element that
originally contained text."
  ;; Check that only rdf:ID and rdf:datatype appear...
  (doalist ((uri value) attributes)
    (declare (ignore value))
    (unless (or (eq uri +rdf-id+) (eq uri +rdf-datatype+))
      (restart-case (error 'prohibited-attribute
                           :attribute uri :source source)
        (ignore-attribute ()
          :report "Ignore the attribute."))))
  ;; ...and then process the property-element.
  (let* ((id (cdr (assoc +rdf-id+ attributes)))
         (datatype (cdr (assoc +rdf-datatype+ attributes)))
         (*object* (if datatype
                     (intern-typed-literal text datatype)
                     (intern-plain-literal text *current-xml-lang*))))
    (emit-triple)
    (when id
      (reify-triple (add-fragment id (klacks:current-xml-base source))))))

(defun empty-property-element-components (source attributes)
  "empty-property-element-components source attributes => pattributes, id, thing
source---a cxml:source
attributes, pattributes---association lists
id---a PURI uri or nil
thing---a indicator of the empty property element
Empty-property-element-components returns as multiple values the
property attributes of the element (as an association list), an id
corresponding to the element (which is NIL if no ID was provided), and
an indicator of the element, which is either a plain literal with the
empty string as its lexical component, a blank node, or a URI."
  (let ((base (klacks:current-xml-base source))
        (property-attributes '())
        (identifiers '())
        (id nil))
    ;; Check that only rdf:ID, rdf:resource, rdf:nodeID, and property
    ;; attributes appear as attributes. At most one of rdf:resource
    ;; and rdf:nodeID should appear. Other attributes are prohibited,
    ;; but can be ignored. (rdf:datatype is prohibited, but a more
    ;; specialized error of type datatyped-empty-property is signalled
    ;; in that case, so that higher-up restarts can handle it.)
    (doalist ((uri value pair) attributes)
      (cond
       ((or (eq uri +rdf-resource+)
            (eq uri +rdf-node-id+))
        (push pair identifiers))
       ;; If ID appears, it must be a valid NCName
       ((eq uri +rdf-id+)
        (do () ((id-name-p value) (setf id value))
          (restart-case (error 'non-nc-name-id
                               :source source
                               :attribute +rdf-id+
                               :value value)
            (ignore-attribute ()
              :report "Ignore the attribute. (Reification will not occur.)"
              (return))
            (nil ()
              :report "Use it anyway."
              (return (setf id value)))
            (use-value (new-value)
              :report "Use a different value."
              :interactive (lambda ()
                             (list (prompt-for-line "Entern a new ID:")))
              (setf value new-value)))))
       ((property-attribute-uri-p uri)
        (push pair property-attributes))
       (t (restart-case (error (if (eq uri +rdf-datatype+)
                                 'datatyped-empty-property
                                 'prohibited-attribute)
                               :attribute uri :source source)
            (ignore-attribute ()
              :report "Ignore the attribute.")))))
    ;; rdf:nodeID and rdf:resource are optional, but mutually
    ;; exclusive. We can ignore them or choose amongst them.
    (do () ((endp (rest identifiers)))
      (with-alist-restarts (id identifiers)
          (restart-case (error 'mutually-exclusive-attributes
                               :source source
                               :attributes identifiers)
            (ignore-attributes ()
              :report "Ignore the mutually exclusive attributes."
              (setf identifiers '())))
        (setf identifiers (list id))))
    ;; The property-attributes are the primary value. If an rdf:ID was
    ;; specified, the appropriate URI is the second value, otherwise
    ;; nil. The third value is the object. If neither rdf:nodeID nor
    ;; rdf:resource were specified, then if there were no
    ;; property-attributes object is a plain literal with empty
    ;; string, and if there were property-attributes, then object is a
    ;; blank node. If rdf:nodeID or rdf:resource were specified, then
    ;; object is a blank node with the specified nodeID, or a URI
    ;; according to rdf:resource.
    (values
     property-attributes
     (if (null id) id (add-fragment id base))
     (if (null identifiers)
       (if (endp property-attributes)
         (intern-plain-literal "" *current-xml-lang*)
         (blank-node))
       (destructuring-bind ((uri . value)) identifiers
         (cond
          ((eq uri +rdf-node-id+) (blank-node value))
          ((eq uri +rdf-resource+) (merge-uris value base))
          ;; This error should be impossible to reach, based on the
          ;; way that the list identifiers is constructed.
          (t (error "Identifiers was non-nil, but the key was neither ~
                     rdf:nodeID nor rdf:resource. Identifiers was ~S."
                    identifiers))))))))

(defun empty-property-element (source attributes)
  "empty-property-element source attributes => |
source---a cxml:source
attributes---an alist
Processes an empty property element, emitting the appropriate triples
for property attributes on the element, as well as the triple of which
the empty property is the object. The triple of which the empty
property element is the object is also reified when the ID attribute
is present."
  (multiple-value-bind (attributes id *object*)
      (empty-property-element-components source attributes)
    (emit-triple)
    (when id (reify-triple id))
    (doalist ((uri value) attributes)
      (if (eq uri +rdf-type+)
        (emit-triple
         :subject *object*
         :predicate +rdf-type+
         :object (intern-uri value))
        (emit-triple
         :subject *object*
         :predicate uri
         :object (intern-plain-literal value *current-xml-lang*))))))
