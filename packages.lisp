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
   #:declare-store-predicate
   #:re-match
   #:do-query
   #:list-store-component))

(defpackage #:juarez.rpc
  (:use #:cl)
  (:export
   #:siyobik-call))

(defpackage #:juarez
  (:use #:cl #:constantia #:alexandria #:split-sequence #:juarez.triples #:juarez.rpc)
  (:export
   #:release-plist
   #:relunp))
