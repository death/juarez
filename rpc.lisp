;;;; +----------------------------------------------------------------+
;;;; | JUAREZ - Warez tools                               DEATH, 2010 |
;;;; +----------------------------------------------------------------+

(in-package #:juarez.rpc)


;;;; Error condition types

(define-condition rpc-client-error (error)
  ((client :initarg :client :reader rpc-client-error-client)))

(define-condition rpc-client-http-error (rpc-client-error)
  ((code :initarg :code :reader rpc-client-http-error-code))
  (:report (lambda (e s)
             (format s "RPC client HTTP error: ~A"
                     (rpc-client-http-error-code e)))))

(define-condition rpc-client-id-mismatch (rpc-client-error)
  ((actual-id :initarg :actual-id :reader rpc-client-id-mismatch-actual-id)
   (expected-id :initarg :expected-id :reader rpc-client-id-mismatch-expected-id))
  (:report (lambda (e s)
             (format s "RPC client ID mismatch:~%  Expected ID: ~A~%  Actual ID: ~A"
                     (rpc-client-id-mismatch-expected-id e)
                     (rpc-client-id-mismatch-actual-id e)))))

(define-condition rpc-client-call-error (rpc-client-error)
  ((info :initarg :info :reader rpc-client-call-error-info))
  (:report (lambda (e s)
             (format s "RPC call failed: ~A"
                     (rpc-client-call-error-info e)))))


;;;; Calls

(defclass rpc-call ()
  ((method-name :initarg :method-name :reader rpc-call-method-name)
   (method-args :initarg :method-args :reader rpc-call-method-args)))


;;;; Client protocol

(defclass rpc-client ()
  ())

(defgeneric rpc-request-response (rpc-client content))
(defgeneric rpc-disconnect (rpc-client))
(defgeneric rpc-encode-call (rpc-client call id))
(defgeneric rpc-encode-multicall (rpc-client calls ids))
(defgeneric rpc-decode-call-response (rpc-client response id))
(defgeneric rpc-decode-multicall-response (rpc-client response ids))
(defgeneric rpc-handle-call-response (rpc-client type value))
(defgeneric rpc-handle-multicall-response (rpc-client response))
(defgeneric rpc-perform-call (rpc-client call))
(defgeneric rpc-perform-multicall (rpc-client calls))


;;;; HTTP transport mixin

(defclass rpc-client-http-mixin ()
  ((certificate :initarg :certificate :accessor rpc-client-certificate)
   (key :initarg :key :accessor rpc-client-key)
   (uri :initarg :uri :accessor rpc-client-uri)
   (stream :initform nil :accessor rpc-client-stream)))

(defmethod rpc-request-response ((client rpc-client-http-mixin) content)
  (multiple-value-bind (response http-code)
      (let ((drakma:*text-content-types* '(("text" . nil)
                                           ("application" . "json-rpc")))
            (drakma:*drakma-default-external-format* :utf-8))
        (cond ((rpc-client-stream client)
               (handler-bind ((error (lambda (e)
                                       (declare (ignore e))
                                       (rpc-disconnect client))))
                 (drakma:http-request (rpc-client-uri client)
                                      :method :post
                                      :content content
                                      :close nil
                                      :stream (rpc-client-stream client))))
              (t (multiple-value-bind (response http-code headers uri stream)
                     (drakma:http-request (rpc-client-uri client)
                                          :certificate (rpc-client-certificate client)
                                          :key (rpc-client-key client)
                                          :method :post
                                          :content content
                                          :close nil)
                   (declare (ignore headers uri))
                   (setf (rpc-client-stream client) stream)
                   (values response http-code)))))
    (unless (eql http-code 200)
      (error 'rpc-client-http-error :client client :code http-code))
    response))

(macrolet ((declare-disconnecting (name)
             `(defmethod (setf ,name) :after (new-value (client rpc-client-http-mixin))
                (rpc-disconnect client))))
  (declare-disconnecting rpc-client-key)
  (declare-disconnecting rpc-client-certificate)
  (declare-disconnecting rpc-client-uri))

(defmethod rpc-disconnect ((client rpc-client-http-mixin))
  (when (rpc-client-stream client)
    (when (open-stream-p (rpc-client-stream client))
      (close (rpc-client-stream client)))
    (setf (rpc-client-stream client) nil)))


;;;; JSON RPC mixin

(defclass rpc-client-json-mixin ()
  ())

(defun make-call-json (call id)
  (list (cons 'jsonrpc "2.0")
        (cons 'id id)
        (cons 'method (rpc-call-method-name call))
        (cons 'params (if (null (rpc-call-method-args call))
                          #()
                          (rpc-call-method-args call)))))

(defmethod rpc-encode-call ((client rpc-client-json-mixin) call id)
  (json:encode-json-to-string (make-call-json call id)))

(defmethod rpc-encode-multicall ((client rpc-client-json-mixin) calls ids)
  (json:encode-json-to-string (mapcar #'make-call-json calls ids)))

(defun make-call-response (client json call-id)
  (with-alist-values ((id error result) json)
    (unless (eql id call-id)
      (error 'rpc-client-id-mismatch :client client :expected-id call-id :actual-id id))
    (if error
        (values :error error)
        (values :result result))))

(defmethod rpc-decode-call-response ((client rpc-client-json-mixin) response id)
  (make-call-response client (json:decode-json-from-string response) id))

(defmethod rpc-decode-multicall-response ((client rpc-client-json-mixin) response ids)
  (mapcar (lambda (call-response call-id)
            (multiple-value-list (make-call-response client call-response call-id)))
          (json:decode-json-from-string response) ids))


;;;; Retryable requests mixin

(defclass rpc-client-retrying-mixin ()
  ())

(defmacro with-retry-restart ((restart-name format-string &rest format-arguments) &body forms)
  (let ((block-name (gensym)))
    `(block ,block-name
       (loop
        (with-simple-restart (,restart-name ,format-string ,@format-arguments)
          (return-from ,block-name ,@forms))))))

(defmethod rpc-perform-call :around ((client rpc-client-retrying-mixin) call)
  (with-retry-restart (rpc-retry-call "Retry RPC call.")
    (call-next-method)))

(defmethod rpc-perform-multicall :around ((client rpc-client-retrying-mixin) calls)
  (with-retry-restart (rpc-retry-call "Retry RPC calls.")
    (call-next-method)))

(defun rpc-retry-call (&optional condition)
  (let ((restart (find-restart 'rpc-retry-call condition)))
    (when restart
      (invoke-restart restart))))


;;;; Basic client implementation

(defclass basic-rpc-client (rpc-client)
  ((next-request-id :initform 0 :accessor rpc-client-next-request-id)))

(defun make-id (client)
  (prog1 (rpc-client-next-request-id client)
    (incf (rpc-client-next-request-id client))))

(defmethod rpc-handle-call-response ((client basic-rpc-client) (type (eql :error)) value)
  (error 'rpc-client-call-error :client client :info value))

(defmethod rpc-handle-call-response ((client basic-rpc-client) (type (eql :result)) value)
  value)

(defmethod rpc-handle-multicall-response ((client basic-rpc-client) response)
  (loop for (type value) in response
        collect (rpc-handle-call-response client type value)))

(defmethod rpc-perform-call ((client basic-rpc-client) call)
  (let ((id (make-id client)))
    (multiple-value-call #'rpc-handle-call-response
      client
      (rpc-decode-call-response client (rpc-request-response client (rpc-encode-call client call id)) id))))

(defmethod rpc-perform-multicall ((client basic-rpc-client) calls)
  (let ((ids (loop for call in calls collect (make-id client))))
    (rpc-handle-multicall-response
     client
     (rpc-decode-multicall-response client (rpc-request-response client (rpc-encode-multicall client calls ids)) ids))))


;;;; Client interface

(defvar *rpc-client*)

(defun make-call-form (spec)
  (destructuring-bind (method-name &rest method-args) spec
    `(make-instance 'rpc-call
                    :method-name ,method-name
                    :method-args (list ,@method-args))))

(defmacro rpc-call (&rest args)
  (when (null args)
    (error "Malformed call form."))
  (let ((client-form (if (and (consp (car args))
                              (eq :client (caar args)))
                         (cadr (pop args))
                         '*rpc-client*)))
    `(rpc-perform-call ,client-form ,(make-call-form args))))

(defmacro rpc-multicall (&rest calls)
  (when (null calls)
    (error "Malformed multicall form."))
  (let ((client-form (if (and (consp (car calls))
                              (eq :client (caar calls)))
                         (cadr (pop calls))
                         '*rpc-client*)))
    `(rpc-perform-multicall ,client-form
                            (list ,@(mapcar #'make-call-form calls)))))

(defmacro with-rpc-client ((var class-name &rest options) &body forms)
  `(let ((,var (make-instance ,class-name ,@options)))
     (unwind-protect (progn ,@forms)
       (rpc-disconnect ,var))))


;;;; Warehouse client

(defclass warehouse-rpc-client (rpc-client-retrying-mixin
                                rpc-client-json-mixin
                                rpc-client-http-mixin
                                basic-rpc-client)
  ())

(defparameter *warehouse-uri* "https://xxxxxxxxxxxx:6780/warehouse")
(defparameter *warehouse-key* "/home/death/lisp/juarez/data/death.pem")
(defparameter *warehouse-certificate* "/home/death/lisp/juarez/data/death.pem")

(defmacro with-warehouse-rpc-client ((&optional (var '*rpc-client*)
                                                (uri '*warehouse-uri*)
                                                (key '*warehouse-key*)
                                                (certificate '*warehouse-certificate*))
                                 &body forms)
  `(with-rpc-client (,var 'warehouse-rpc-client :uri ,uri :key ,key :certificate ,certificate)
     ,@forms))
