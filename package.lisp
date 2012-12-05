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

(in-package "COMMON-LISP")

(defpackage #:cl-rdfxml 
  (:use "COMMON-LISP")
  (:documentation
   "The cl-rdfxml package provides functionality for processing
RDF/XML representations of RDF graphs. RDF/XML processes XML using
Closure XML (CXML) and handles URIs using PURI.")
  (:export
   ;; main parsing interface
   #:parse-document
   ;; blank nodes
   #:blank-node
   #:blank-node-id
   ;; literals
   #:literal
   #:literal-string
   #:plain-literal
   #:literal-language
   #:intern-plain-literal
   #:typed-literal
   #:literal-datatype
   #:intern-typed-literal
   ;; conditions
   #:rdfxml-warning                     ; warnings
   #:rdf-prefixed-non-rdf-name
   #:*warn-on-rdf-prefixed-non-rdf-names*
   #:non-namespaced-name
   #:*warn-on-non-namespaced-names*
   #:other-parse-type
   #:*warn-on-parse-type-other*
   #:rdfxml-error                       ; errors
   #:illegal-namespace-name
   #:invalid-attribute-value
   #:repeated-id
   #:non-nc-name-id
   #:invalid-language-tag
   #:unexpected-characters
   #:duplicate-attribute
   #:prohibited-attribute
   #:datatyped-empty-property
   #:*coerce-datatyped-empty-properties*
   #:non-namespaced-attribute
   #:mutually-exclusive-attributes
   #:bad-uri
   #:bad-property-element-uri
   #:bad-node-element-uri
   #:*parse-uris-strictly* ; for puri:uri-parse-error
   ;; restarts (without restart-functions)
   #:use-old-value
   #:use-new-value
   #:choose-identifier
   ;; restart functions
   #:ignore-language
   #:ignore-attribute
   #:ignore-attributes
   #:ignore-characters
   #:parse-as-typed-literal
   #:parse-uri-non-strictly
   ;; constants
   #:+rdf-namespace+
   #:+rdfs-namespace+
   #:+rdfs-resource+ 
   #:+rdfs-literal+ 
   #:+rdf-xml-literal+ 
   #:+rdfs-class+ 
   #:+rdf-property+ 
   #:+rdfs-datatype+ 
   #:+rdf-statement+ 
   #:+rdf-bag+ 
   #:+rdf-seq+ 
   #:+rdf-alt+ 
   #:+rdfs-container+ 
   #:+rdfs-container-membership-property+ 
   #:+rdf-list+ 
   #:+rdf-type+ 
   #:+rdfs-subclass-of+ 
   #:+rdfs-subproperty-of+ 
   #:+rdfs-domain+ 
   #:+rdfs-range+ 
   #:+rdfs-label+ 
   #:+rdfs-comment+ 
   #:+rdfs-member+ 
   #:+rdfs-see-also+ 
   #:+rdfs-is-defined-by+ 
   #:+rdf-first+ 
   #:+rdf-rest+ 
   #:+rdf-value+ 
   #:+rdf-subject+ 
   #:+rdf-predicate+ 
   #:+rdf-object+ 
   #:+rdf-rdf+ 
   #:+rdf-description+ 
   #:+rdf-id+ 
   #:+rdf-about+ 
   #:+rdf-parse-type+ 
   #:+rdf-resource+ 
   #:+rdf-li+ 
   #:+rdf-node-id+ 
   #:+rdf-datatype+ 
   #:+rdf-nil+ 
   #:+rdf-bag-id+ 
   #:+rdf-about-each+ 
   #:+rdf-about-each-prefix+))
