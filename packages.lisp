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

(defpackage #:juarez
  (:use #:cl #:constantia #:alexandria #:split-sequence #:juarez.prolog)
  (:export
   #:release-plist
   #:relunp))
