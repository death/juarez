;;;; +----------------------------------------------------------------+
;;;; | JUAREZ - Warez tools                               DEATH, 2010 |
;;;; +----------------------------------------------------------------+

;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-


;;;; System definitions

(in-package #:cl-user)

(asdf:defsystem #:juarez
  :description "Warez tools"
  :depends-on (#:alexandria
               #:cl-fad
               #:constantia
               #:split-sequence)
  :serial t
  :components
  ((:file "packages")
   (:file "utils")
   (:file "release-plist")
   (:file "prolog")
   (:file "database")
   (:file "relunp")))
