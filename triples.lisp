;;;; +----------------------------------------------------------------+
;;;; | JUAREZ - Warez tools                               DEATH, 2010 |
;;;; +----------------------------------------------------------------+

(in-package #:juarez.triples)


(defvar *store* nil)

(defvar *all-stores*
  (make-hash-table))

(defclass store ()
  ((name :initarg :name :accessor store-name)
   (num-triples :initform 0 :accessor store-num-triples)
   (spo :initform (make-hash-table :test 'equal) :accessor store-spo)
   (pos :initform (make-hash-table :test 'equal) :accessor store-pos)
   (osp :initform (make-hash-table :test 'equal) :accessor store-osp)))

(defmethod print-object ((store store) stream)
  (print-unreadable-object (store stream :type t)
    (format stream "~S" (store-name store)))
  store)

(defun make-store (name)
  (when (gethash name *all-stores*)
    (cerror "Overwrite store." "A store named ~S already exists." name))
  (setf (gethash name *all-stores*)
        (make-instance 'store :name name)))

(defun find-store (name)
  (if (typep name 'store)
      name
      (or (gethash name *all-stores*)
          (progn
            (cerror "Create a new store." "A store named ~S doesn't exist." name)
            (make-store name)))))

(defmacro ensure-store (place)
  ;; bad macro
  `(setf ,place (find-store ,place)))

(defmacro in-store (name)
  `(setf *store* (find-store ',name)))

(defun add (sub pred obj &optional (graph *store*))
  (ensure-store graph)
  (add-to-index (store-spo graph) sub pred obj)
  (add-to-index (store-pos graph) pred obj sub)
  (add-to-index (store-osp graph) obj sub pred)
  (incf (store-num-triples graph))
  (values))

(defun add-to-index (index a b c)
  (multiple-value-bind (a-val a-found) (gethash a index)
    (unless a-found
      (setf a-val (setf (gethash a index) (make-hash-table :test 'equal))))
    (pushnew c (gethash b a-val '()) :test #'equal)))

(defun del (sub pred obj &optional (graph *store*))
  (ensure-store graph)
  (dolist (triple (triples graph sub pred obj))
    (destructuring-bind (del-sub del-pred del-obj) triple
      (remove-from-index (store-spo graph) del-sub del-pred del-obj)
      (remove-from-index (store-pos graph) del-pred del-obj del-sub)
      (remove-from-index (store-osp graph) del-obj del-sub del-pred))
    (decf (store-num-triples graph)))
  (values))

(defun remove-from-index (index a b c)
  (multiple-value-bind (a-val a-found) (gethash a index)
    (when a-found
      (multiple-value-bind (b-val b-found) (gethash b a-val)
        (when b-found
          (when (member c b-val :test #'equal)
            (setf (gethash b a-val)
                  (remove c b-val :test #'equal)))
          (when (null (gethash b index))
            (remhash b a-val))))
      (when (zerop (hash-table-count a-val))
        (remhash a index)))))

(defun write-store (filename &optional (graph *store*))
  (ensure-store graph)
  (with-open-file (out filename
                       :direction :output
                       :if-exists :supersede
                       :if-does-not-exist :create)
    (write-store-to-stream graph out)))

(defun write-store-to-stream (graph stream)
  (with-standard-io-syntax
    (print (triples t t t graph) stream))
  (values))

(defun read-store (filename &optional (graph *store*))
  (ensure-store graph)
  (with-open-file (in filename
                      :direction :input)
    (read-store-from-stream graph in)))

(defun read-store-from-stream (graph stream)
  (with-standard-io-syntax
    (loop for (sub pred obj) in (read stream)
          do (add sub pred obj graph))))

(defun wildcard-p (x)
  (eq x 't))

(defun map-triples (function sub pred obj &optional (graph *store*))
  (ensure-store graph)
  (cond ((wildcard-p sub)
         (cond ((wildcard-p pred)
                (cond ((wildcard-p obj)
                       ;; T T T
                       (maphash (lambda (ret-sub pred-table)
                                  (maphash (lambda (ret-pred obj-set)
                                             (dolist (ret-obj obj-set)
                                               (funcall function ret-sub ret-pred ret-obj)))
                                           pred-table))
                                (store-spo graph)))
                      (t
                       ;; T T X
                       (multiple-value-bind (sub-table found) (gethash obj (store-osp graph))
                         (when found
                           (maphash (lambda (ret-sub pred-set)
                                      (dolist (ret-pred pred-set)
                                        (funcall function ret-sub ret-pred obj)))
                                    sub-table))))))
               (t
                (cond ((wildcard-p obj)
                       ;; T X T
                       (multiple-value-bind (obj-table found) (gethash pred (store-pos graph))
                         (when found
                           (maphash (lambda (ret-obj sub-set)
                                      (dolist (ret-sub sub-set)
                                        (funcall function ret-sub pred ret-obj)))
                                    obj-table))))
                      (t
                       ;; T X Y
                       (multiple-value-bind (obj-table found1) (gethash pred (store-pos graph))
                         (when found1
                           (multiple-value-bind (sub-set found2) (gethash obj obj-table)
                             (when found2
                               (dolist (ret-sub sub-set)
                                 (funcall function ret-sub pred obj)))))))))))
        (t
         (cond ((wildcard-p pred)
                (cond ((wildcard-p obj)
                       ;; X T T
                       (multiple-value-bind (pred-table found) (gethash sub (store-spo graph))
                         (when found
                           (maphash (lambda (ret-pred obj-set)
                                      (dolist (ret-obj obj-set)
                                        (funcall function sub ret-pred ret-obj)))
                                    pred-table))))
                      (t
                       ;; X T X
                       (multiple-value-bind (sub-table found1) (gethash obj (store-osp graph))
                         (when found1
                           (multiple-value-bind (pred-set found2) (gethash sub sub-table)
                             (when found2
                               (dolist (ret-pred pred-set)
                                 (funcall function sub ret-pred obj)))))))))
               (t
                (cond ((wildcard-p obj)
                       ;; X X T
                       (multiple-value-bind (pred-table found1) (gethash sub (store-spo graph))
                         (when found1
                           (multiple-value-bind (obj-set found2) (gethash pred pred-table)
                             (when found2
                               (dolist (ret-obj obj-set)
                                 (funcall function sub pred ret-obj)))))))
                      (t
                       ;; X X X
                       (multiple-value-bind (pred-table found1) (gethash sub (store-spo graph))
                         (when found1
                           (multiple-value-bind (obj-set found2) (gethash pred pred-table)
                             (when found2
                               (when (member obj obj-set :test #'equal)
                                 (funcall function sub pred obj)))))))))))))

(defun triples (&optional (sub t) (pred t) (obj t) (graph *store*))
  (let ((result '()))
    (map-triples (lambda (ret-sub ret-pred ret-obj)
                   (push (list ret-sub ret-pred ret-obj) result))
                 sub pred obj graph)
    result))

(defun value (&optional (sub t) (pred t) (obj t) (graph *store*))
  (map-triples (lambda (ret-sub ret-pred ret-obj)
                 (when (wildcard-p sub) (return-from value (values ret-sub t)))
                 (when (wildcard-p pred) (return-from value (values ret-pred t)))
                 (when (wildcard-p obj) (return-from value (values ret-obj t))))
               sub pred obj graph)
  (values nil nil))

(defun clear (&optional (graph *store*))
  (ensure-store graph)
  (clrhash (store-spo graph))
  (clrhash (store-osp graph))
  (clrhash (store-pos graph))
  (setf (store-num-triples graph) 0)
  (values))

(defun read-store-csv (filename &optional (graph *store*))
  (ensure-store graph)
  (with-open-file (in filename :direction :input)
    (let ((strings (make-hash-table :test 'equal)))
      (flet ((intern-string (x)
               (or (gethash x strings)
                   (setf (gethash x strings) x))))
        (loop for line = (read-line in nil nil)
              while line
              do (destructuring-bind (sub pred obj)
                     (arnesi:parse-csv-string (string-trim '(#\Return #\Space #\Newline) line))
                   (add (intern-string sub)
                        (intern-string pred)
                        (intern-string obj)
                        graph)))))))

(defun var-p (x)
  (and (symbolp x)
       (char= #\? (char (symbol-name x) 0))))

(defun query (clauses &optional (store *store*))
  (ensure-store store)
  (%query clauses '() store))

(defun %query (clauses bindings-set store)
  (if (null clauses)
      bindings-set
      (%query (cdr clauses)
              (%query-1 (car clauses) bindings-set store)
              store)))

(defun %query-1 (clause bindings-set store)
  (let ((rows (triples-for-clause clause store)))
    (if (null bindings-set)
        (mapcar (lambda (row)
                  (extract-bindings clause row))
                rows)
        (loop for bindings in bindings-set
              nconc (loop for row in rows
                          when (merge-bindings bindings clause row)
                          collect it)))))

(defun extract-bindings (clause row)
  (destructuring-bind (c-sub c-pred c-obj) clause
    (destructuring-bind (sub pred obj) row
      (nconc (when (var-p c-sub) (list (cons c-sub sub)))
             (when (var-p c-pred) (list (cons c-pred pred)))
             (when (var-p c-obj) (list (cons c-obj obj)))))))

(defun merge-bindings (bindings clause row)
  (mapc (lambda (c r)
          (when (var-p c)
            (cond ((assoc c bindings)
                   (unless (equal (cdr (assoc c bindings)) r)
                     (return-from merge-bindings nil)))
                  (t (push (cons c r) bindings)))))
        clause row)
  bindings)

(defun triples-for-clause (clause store)
  (destructuring-bind (c-sub c-pred c-obj) clause
    (triples (if (var-p c-sub) t c-sub)
             (if (var-p c-pred) t c-pred)
             (if (var-p c-obj) t c-obj)
             store)))

(defun apply-inference (rule &optional (store *store*))
  (dolist (bindings (rule-bindings-set rule store))
    (dolist (triple (rule-make-triples* rule bindings))
      (destructuring-bind (sub pred obj) triple
        (add sub pred obj store)))))

(defgeneric rule-queries (rule))

(defun rule-bindings-set (rule store)
  (mapcan (lambda (q) (query q store)) (rule-queries rule)))

(defclass inference-rule ()
  ())

(defmethod rule-queries ((rule inference-rule)) '())

(defgeneric rule-make-triples (rule &key &allow-other-keys))

(defun rule-make-triples* (rule bindings)
  (apply #'rule-make-triples rule
         (keyargs-from-bindings bindings)))

(defun keyargs-from-bindings (bindings)
  (loop for (key . val) in bindings
        collect (keyword-from-var key)
        collect val))

(defun keyword-from-var (var)
  (intern (subseq (symbol-name var) 1)
          (load-time-value (find-package "KEYWORD"))))

(defun store-predicate (store-name)
  (and (symbolp store-name)
       (get store-name 'store-predicate)))

(defmacro declare-store-predicate (lisp-name &optional (store-name lisp-name))
  `(progn
     (setf (get ',store-name 'store-predicate) ',lisp-name)
     ',store-name))

(declare-store-predicate =)
(declare-store-predicate <)
(declare-store-predicate >)
(declare-store-predicate <=)
(declare-store-predicate >=)
(declare-store-predicate /=)

#+cl-ppcre
(declare-store-predicate re-match)
#+cl-ppcre
(defun re-match (lhs rhs)
  (ppcre:scan (ppcre:create-scanner rhs :case-insensitive-mode t) lhs))

(defun compile-query (clauses bound-vars form store)
  (if (null clauses)
      form
      (multiple-value-bind (new-bound-vars form-lambda)
          (compile-clause (first clauses) bound-vars store)
        (funcall form-lambda
                 (compile-query (rest clauses) new-bound-vars form store)))))

(defun compile-clause (clause bound-vars store)
  (flet ((unbound-var-p (x)
           (and (var-p x)
                (not (member x bound-vars)))))
    (macrolet ((match (&body match-clauses)
                 `(cond
                    ,@(loop for (booleans action) in match-clauses
                            collect `((and ,@(mapcar (lambda (boolean part)
                                                       (if boolean
                                                           `(not (unbound-var-p ,part))
                                                           `(unbound-var-p ,part)))
                                                     booleans '(sub pred obj)))
                                      (lambda (form)
                                        ,action))))))
      (destructuring-bind (sub pred obj) clause
        (values
         (nconc (remove-if-not #'unbound-var-p clause) bound-vars)
         (with-gensyms (sub-table sub-set pred-table pred-set obj-table obj-set found found1 found2)
           (match
               ((nil nil nil)
                `(maphash (lambda (,sub ,pred-table)
                            (maphash (lambda (,pred ,obj-set)
                                       (dolist (,obj ,obj-set)
                                         ,form))
                                     ,pred-table))
                          (store-spo ,store)))
             ((nil nil t)
              `(multiple-value-bind (,sub-table ,found) (gethash ,obj (store-osp ,store))
                 (when ,found
                   (maphash (lambda (,sub ,pred-set)
                              (dolist (,pred pred-set)
                                ,form))
                            ,sub-table))))
             ((nil t nil)
              `(multiple-value-bind (,obj-table ,found) (gethash ,pred (store-pos ,store))
                 (when ,found
                   (maphash (lambda (,obj ,sub-set)
                              (dolist (,sub ,sub-set)
                                ,form))
                            ,obj-table))))
             ((nil t t)
              `(multiple-value-bind (,obj-table ,found1) (gethash ,pred (store-pos ,store))
                 (when ,found1
                   (multiple-value-bind (,sub-set ,found2) (gethash ,obj ,obj-table)
                     (when ,found2
                       (dolist (,sub ,sub-set)
                         ,form))))))
             ((t nil nil)
              `(multiple-value-bind (,pred-table ,found) (gethash ,sub (store-spo ,store))
                 (when ,found
                   (maphash (lambda (,pred ,obj-set)
                              (dolist (,obj ,obj-set)
                                ,form))
                            ,pred-table))))
             ((t nil t)
              `(multiple-value-bind (,sub-table ,found1) (gethash ,obj (store-osp ,store))
                 (when ,found1
                   (multiple-value-bind (,pred-set ,found2) (gethash ,sub ,sub-table)
                     (when ,found2
                       (dolist (,pred ,pred-set)
                         ,form))))))
             ((t t nil)
              `(multiple-value-bind (,pred-table ,found1) (gethash ,sub (store-spo ,store))
                 (when ,found1
                   (multiple-value-bind (,obj-set ,found2) (gethash ,pred ,pred-table)
                     (when ,found2
                       (dolist (,obj ,obj-set)
                         ,form))))))
             ((t t t)
              (if-let (fn (store-predicate pred))
                `(when (,fn ,sub ,obj)
                   ,form)
                `(multiple-value-bind (,pred-table ,found1) (gethash ,sub (store-spo ,store))
                   (when ,found1
                     (multiple-value-bind (,obj-set ,found2) (gethash ,pred ,pred-table)
                       (when ,found2
                         (when (member ,obj ,obj-set :test #'equal)
                           ,form))))))))))))))

(defmacro do-query ((clauses &optional (store '*store*)) &body forms)
  (once-only (store)
    (compile-query clauses '() `(progn ,@forms) store)))
