;;;; +----------------------------------------------------------------+
;;;; | JUAREZ - Warez tools                               DEATH, 2010 |
;;;; +----------------------------------------------------------------+

(in-package #:cl-user)


;;;; Package definitions

(defpackage #:juarez.utils
  (:use #:cl #:alexandria #:constantia)
  (:export
   #:keywordify-words
   #:keywordify
   #:char-match
   #:with-alist-values
   #:alist-values-lister
   #:approximate-size
   #:format-date
   #:eval-always))

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
  (:import-from #:juarez.utils #:with-alist-values)
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

(defpackage #:juarez.notifications
  (:use #:cl)
  (:import-from #:alexandria #:deletef #:if-let #:with-gensyms #:make-keyword
                #:format-symbol)
  (:import-from #:usocket #:socket-connect #:socket-stream #:socket-close)
  (:import-from #:cl+ssl #:make-ssl-client-stream)
  (:import-from #:json #:decode-json-from-string #:encode-json-to-string)
  (:import-from #:babel #:octets-to-string #:string-to-octets #:string-size-in-octets)
  (:import-from #:juarez.utils #:with-alist-values #:eval-always)
  (:export
   #:notification-client
   #:notification-client-host
   #:notification-client-port
   #:notification-client-ssl-key
   #:notification-client-ssl-certificate
   #:open-notification-client
   #:close-notification-client
   #:add-notification-watcher
   #:remove-notification-watcher
   #:notification-event-dispatch
   #:dispatch-notification
   #:notification-client-rpc
   #:grab-notification-client-rpc-id
   #:with-open-notification-client
   #:basic-notification-client
   #:notification-message
   #:notification-time
   #:notification-type
   #:notification-content
   #:notification-client-error
   #:notification-client-error-message
   #:notification-client-method-error
   #:get-notifications
   #:get-notifications-count
   #:generate-notification
   #:get-torrents
   #:download-torrent-by-id
   #:get-site-statistics
   #:search-release
   #:search-release-re
   #:make-warehouse-notification-client))
  
(defpackage #:juarez
  (:use #:cl #:constantia #:alexandria #:split-sequence #:juarez.utils #:juarez.triples #:juarez.rpc #:juarez.notifications)
  (:export
   #:release-plist
   #:relunp
   #:delete-blacklisted))
