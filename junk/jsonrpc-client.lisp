(defpackage #:jsonrpc
  (:use #:cl)
  (:export
   #:*version*
   #:*client*
   #:client-error
   #:client-http-error
   #:client-http-error-code
   #:client-id-mismatch
   #:client-id-mismatch-actual-id
   #:client-id-mismatch-expected-id
   #:client-call-error
   #:client-call-error-info
   #:client
   #:client-certificate
   #:client-key
   #:client-uri
   #:client-disconnect
   #:call*
   #:multicall*
   #:call
   #:multicall
   #:with-client))

(in-package #:jsonrpc)

(defparameter *version* "2.0")

(define-condition client-error (error)
  ())

(define-condition client-http-error (client-error)
  ((code :initarg :code :reader client-http-error-code)))

(define-condition client-id-mismatch (client-error)
  ((actual-id :initarg :actual-id :reader client-id-mismatch-actual-id)
   (expected-id :initarg :expected-id :reader client-id-mismatch-expected-id)))

(define-condition client-call-error (client-error)
  ((info :initarg :info :reader client-call-error-info)))

(defclass client ()
  ((next-request-id :initform 0 :accessor client-next-request-id)
   (certificate :initarg :certificate :accessor client-certificate)
   (key :initarg :key :accessor client-key)
   (uri :initarg :uri :accessor client-uri)
   (stream :initform nil :accessor client-stream)))

(macrolet ((declare-disconnecting (name)
             `(defmethod (setf ,name) :after (new-value (client client))
                (client-disconnect client))))
  (declare-disconnecting client-key)
  (declare-disconnecting client-certificate)
  (declare-disconnecting uri))

(defun make-id (client)
  (prog1 (client-next-request-id client)
    (incf (client-next-request-id client))))

(defun make-request-json (method params id)
  (check-type method string)
  (list (cons 'jsonrpc *version*)
        (cons 'id id)
        (cons 'method method)
        (cons 'params (if (null params)
                          #()
                          params))))

(defmacro with-retry-restart ((restart-name format-string &rest format-arguments) &body forms)
  (let ((block-name (gensym)))
    `(block ,block-name
       (loop
        (with-simple-restart (,restart-name ,format-string ,@format-arguments)
          (return-from ,block-name ,@forms))))))

(defun dispatch-request (json-string client)
  (with-retry-restart (retry-call "Try calling the method again.")
    (multiple-value-bind (unparsed-response http-code)
        (cond ((client-stream client)
               (handler-case
                   (drakma:http-request (client-uri client)
                                        :method :post
                                        :content json-string
                                        :close nil
                                        :stream (client-stream client))
                 (error (e)
                   (client-disconnect client :abort t)
                   (error e))))
              (t (let ((drakma::*ssl-certificate* (client-certificate client))
                       (drakma::*ssl-key* (client-key client)))
                   (multiple-value-bind (unparsed-response http-code headers uri stream)
                       (drakma:http-request (client-uri client)
                                            :method :post
                                            :content json-string
                                            :close nil)
                     (declare (ignore headers uri))
                     (setf (client-stream client) stream)
                     (values unparsed-response http-code)))))
        (unless (eql http-code 200)
          (error 'client-http-error :code http-code))
        (etypecase unparsed-response
          (string unparsed-response)
          (vector (babel:octets-to-string unparsed-response)))))))

(defun handle-response (response request-id)
  (juarez::with-alist-values ((id error result) response :keywords t)
    (unless (eql id request-id)
      (error 'client-id-mismatch :expected-id request-id :actual-id id))
    (when error
      (error 'client-call-error :info error))
    result))

(defun handle-responses (responses request-ids)
  (mapcar #'handle-response responses request-ids))

(defun client-disconnect (client &key abort)
  (when (client-stream client)
    (when (open-stream-p (client-stream client))
      (close (client-stream client) :abort abort))
    (setf (client-stream client) nil)))

(defvar *client*)

(defun call* (method-and-args &optional (client *client*))
  (let ((id (make-id client)))
    (handle-response
     (json:decode-json-from-string
      (dispatch-request
       (json:encode-json-to-string
        (make-request-json (car method-and-args) (cdr method-and-args) id))
       client))
     id)))

(defun multicall* (calls &optional (client *client*))
  (let ((ids (loop for call in calls collect (make-id client))))
    (let ((requests (mapcar (lambda (call id) (make-request-json (car call) (cdr call) id)) calls ids)))
      (handle-responses
       (json:decode-json-from-string
        (dispatch-request
         (json:encode-json-to-string requests)
         client))
       ids))))

(defmacro call (&rest args)
  (when (null args)
    (error "Malformed call form."))
  (let ((client-form (if (and (consp (car args))
                              (eq :client (caar args)))
                         (cadr (pop args))
                         '*client*)))
    `(call* (list ,@args) ,client-form)))

(defmacro multicall (&rest calls)
  (when (null calls)
    (error "Malformed multicall form."))
  (let ((client-form (if (and (consp (car calls))
                              (eq :client (caar calls)))
                         (cadr (pop calls))
                         '*client*)))
    `(multicall* (list ,@(mapcar (lambda (call) `(list ,@call)) calls))
                 ,client-form)))

(defmacro with-client ((&key var uri certificate key) &body forms)
  (when (null var)
    (setf var '*client*))
  (when (null uri)
    (error "RPC client URI was not supplied."))
  `(let ((,var (make-instance 'client :uri ,uri :certificate ,certificate :key ,key)))
     (unwind-protect (progn ,@forms)
       (client-disconnect ,var))))

