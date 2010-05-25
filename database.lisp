;;;; +----------------------------------------------------------------+
;;;; | JUAREZ - Warez tools                               DEATH, 2010 |
;;;; +----------------------------------------------------------------+

(in-package #:juarez)


;;;; Read data from qto's dump

(defun parse-fields (field-specs string &key (delimiter #\Tab))
  (translate-fields field-specs (split-sequence delimiter string)))

(defun translate-fields (field-specs field-strings)
  (mapcar (lambda (spec string)
            (cons (car spec)
                  (field-string-to-parsed-type string (cadr spec))))
          field-specs field-strings))

(defun field-string-to-parsed-type (string type)
  (ecase type
    (string string)
    (integer (parse-integer string))
    (keyword (keywordify string))
    (release-name (list* :raw-name string (release-plist string)))
    (date (parse-datestring string))))

(defun parse-datestring (string)
  (loop for (s e) in '((0 4) (5 7) (8 10) (11 13) (14 16) (17 19))
        collect (parse-integer string :start s :end e)))

(defun release-facts (release)
  (with-alist-values ((id name release-date release-size section-name) release)
    `((title ,id ,(getf name :title))
      (raw-name ,id ,(getf name :raw-name))
      (group ,id ,(intern (string-upcase (getf name :group)) :keyword))
      (section ,id ,section-name)
      ,@(when (consp release-date)
          `((date ,id ,release-date)))
      (size ,id ,release-size)
      ,@(loop for quality in (getf name :properties)
              collect (list 'property id quality)))))

(defun parse-scene-access (line)
  (parse-fields
   '((id integer)
     (site-id integer)
     (torrent-path string)
     (section-name keyword)
     (name release-name)
     (info-hash string)
     (pre-time string)
     (file-count integer)
     (release-date date)
     (release-size integer)
     (hit-count integer)
     (download-count integer)
     (seeder-count integer)
     (leecher-count integer))
   line))

(defun parse-torrent-vault (line)
  (parse-fields
   '((id integer)
     (site-id integer)
     (torrent-path string)
     (section-name keyword)
     (name release-name)
     (pre-time string)
     (genre string)
     (release-date string)
     (added date)
     (release-date-offset string)
     (release-size integer)
     (download-count integer)
     (seeder-count integer)
     (leecher-count integer)
     (uploader string))
   line))

(defun skip-until-copy (stream)
  (loop for line = (read-line stream)
        until (starts-with-subseq "COPY " line)))

(defun output-form (form out)
  (write form :escape t :readably t :pretty nil :stream out)
  (terpri out))

(defvar *dump-filename*
  "/home/death/lisp/juarez/data/warehouse.dump")

(defun map-releases-in-dump (function &optional (input *dump-filename*))
  (with-open-file (in input :direction :input)
    (flet ((parse (how)
             (skip-until-copy in)
             (loop for line = (read-line in)
                   while (not (equal line "\\."))
                   do (with-simple-restart (continue "Skip this line from dump.")
                        (funcall function (release-facts (funcall how line)))))))
      (parse #'parse-scene-access)
      (parse #'parse-torrent-vault))))

(defmacro do-releases-in-dump ((release-var &optional (input '*dump-filename*)) &body forms)
  `(block nil
     (map-releases-in-dump
      (lambda (,release-var)
        ,@forms)
      ,input)))


;;;; Prolog database

(defun output-facts (facts out)
  (dolist (fact facts)
    (output-form `(prolog:<- ,fact) out)))

(defun generate-prolog-facts-db (&key (input *dump-filename*) (output "/home/death/lisp/juarez/data/facts.lisp"))
  (with-open-file (out output
                       :direction :output
                       :if-exists :supersede
                       :if-does-not-exist :create)
    (output-form `(cl:in-package :juarez) out)
    (do-releases-in-dump (release input)
      (output-facts release out))))

(prolog:<- (= ?x ?x))
(prolog:declare-prolog-predicate <)
(prolog:declare-prolog-predicate >)
(prolog:declare-prolog-predicate <=)
(prolog:declare-prolog-predicate >=)
(prolog:declare-prolog-predicate /=)
(prolog:declare-prolog-predicate date<=)

(defun date<= (a b) (every #'<= a b))


;;;; Triples database

(defun add-dump-to-triples (&key (input *dump-filename*) (store triples:*store*) (compact t))
  (when (null store)
    (setf store (make-store 'juarez)))
  (let ((strings (make-hash-table :test 'equal)))
    (flet ((maybe-intern-string (x)
             (typecase x
               (string
                (or (gethash x strings)
                    (setf (gethash x strings) x)))
               (t x))))
      (do-releases-in-dump (release input)
        (dolist (fact release)
          (destructuring-bind (pred sub obj) fact
            (when compact
              (setf pred (maybe-intern-string pred))
              (setf sub (maybe-intern-string sub))
              (setf obj (maybe-intern-string obj)))
            (triples:add sub pred obj store)))))))
