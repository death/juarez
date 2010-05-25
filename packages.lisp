;;;; +----------------------------------------------------------------+
;;;; | JUAREZ - Warez tools                               DEATH, 2010 |
;;;; +----------------------------------------------------------------+

(in-package #:cl-user)


;;;; Package definitions

(defpackage #:juarez.prolog
  (:use #:cl)
  (:nicknames #:prolog)
  (:export
   #:?-
   #:<-
   #:clear-db
   #:clear-predicate
   #:do-answers
   #:add-clause
   #:add-facts
   #:declare-prolog-predicate))

(defpackage #:juarez.triples
  (:use #:cl #:alexandria)
  (:nicknames #:triples)
  (:export
   #:*store*
   #:store-name
   #:store-num-triples
   #:make-store
   #:find-store
   #:in-store
   #:add
   #:del
   #:write-store
   #:read-store
   #:map-triples
   #:triples
   #:value
   #:clear
   #:read-store-csv
   #:query
   #:apply-inference
   #:inference-rule
   #:rule-queries
   #:make-rule-triples
   #:declare-triples-predicate
   #:re-match
   #:do-query))

(defpackage #:juarez
  (:use #:cl #:constantia #:alexandria #:split-sequence #:juarez.triples)
  (:export
   #:release-plist
   #:relunp))
