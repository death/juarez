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
   ;; Error stuff
   #:rpc-client-error
   #:rpc-client-error-client
   #:rpc-client-http-error
   #:rpc-client-http-error-code
   #:rpc-client-id-mismatch
   #:rpc-client-id-mismatch-actual-id
   #:rpc-client-id-mismatch-expected-id
   #:rpc-client-call-error
   #:rpc-client-call-error-info
   ;; Calls
   #:rpc-call
   #:rpc-call-method-name
   #:rpc-call-method-args
   ;; Client protocol
   #:rpc-client
   #:rpc-request-response
   #:rpc-disconnect
   #:rpc-encode-call
   #:rpc-encode-multicall
   #:rpc-decode-call-response
   #:rpc-decode-multicall-response
   #:rpc-handle-call-response
   #:rpc-handle-multicall-response
   #:rpc-perform-call
   #:rpc-perform-multicall
   ;; HTTP transport mixin
   #:rpc-client-http-mixin
   #:rpc-client-certificate
   #:rpc-client-key
   #:rpc-client-uri
   ;; JSON RPC mixin
   #:rpc-client-json-mixin
   ;; Retryable requests mixin
   #:rpc-client-retrying-mixin
   #:rpc-retry-request
   ;; Basic client implementation
   #:basic-rpc-client
   ;; Client interface
   #:*rpc-client*
   #:rpc-call
   #:rpc-multicall
   #:with-rpc-client
   ;; Warehouse client
   #:warehouse-rpc-client
   #:with-warehouse-rpc-client))

(defpackage #:juarez
  (:use #:cl #:constantia #:alexandria #:split-sequence #:juarez.triples #:juarez.rpc)
  (:export
   #:release-plist
   #:relunp))
