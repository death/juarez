;;;; +----------------------------------------------------------------+
;;;; | JUAREZ - Warez tools                               DEATH, 2010 |
;;;; +----------------------------------------------------------------+

(in-package #:juarez.rpc)


;;;; JSON RPC API to qto's warehouse

(defparameter *jsonrpc-version* "2.0"
  "A string specifying the version of the JSONRPC protocol.")

(defvar *jsonrpc-id* 0
  "An integer specifying the next ID to be used.")

(defvar *jsonrpc-certificate* nil
  "The path to a PEM file to use for SSL certificate.")

(defvar *jsonrpc-key* nil
  "The path to a PEM file to use for SSL key.")

(defvar *jsonrpc-uri* nil
  "A URI string for the JSONRPC HTTP service.")

(define-condition jsonrpc-error (error)
  ())

(define-condition jsonrpc-http-error (jsonrpc-error)
  ((code :initarg :code :reader jsonrpc-http-error-code))
  (:report report-jsonrpc-http-error))

(defun report-jsonrpc-http-error (condition stream)
  (format stream "JSONRPC HTTP Error: ~A."
          (jsonrpc-http-error-code condition)))

(define-condition jsonrpc-id-mismatch (jsonrpc-error)
  ((expected-id :initarg :expected-id :reader jsonrpc-id-mismatch-expected-id)
   (actual-id :initarg :actual-id :reader jsonrpc-id-mismatch-actual-id))
  (:report report-jsonrpc-id-mismatch))

(defun report-jsonrpc-id-mismatch (condition stream)
  (format stream "Expected JSONRPC ID ~A, but got ID ~A."
          (jsonrpc-id-mismatch-expected-id condition)
          (jsonrpc-id-mismatch-actual-id condition)))

(define-condition jsonrpc-method-error (jsonrpc-error)
  ((info :initarg :info :reader jsonrpc-method-error-info)))

(defun make-id ()
  "Return a fresh ID."
  (prog1 *jsonrpc-id*
    (incf *jsonrpc-id*)))

(defun make-request (method params &optional id)
  "Create a fresh JSONRPC request object."
  (check-type method string)
  (list (cons 'jsonrpc *jsonrpc-version*)
        (cons 'id id)
        (cons 'method method)
        (cons 'params
              (if (null params)
                  #()
                  params))))

(defun request-over-http (json-string)
  "Post a JSON object encoded as string and return the reply."
  (let ((drakma::*ssl-certificate* *jsonrpc-certificate*)
        (drakma::*ssl-key* *jsonrpc-key*))
    (multiple-value-bind (unparsed-response http-code)
        (drakma:http-request *jsonrpc-uri* :method :post :content json-string)
      (unless (eql http-code 200)
        (error 'jsonrpc-http-error :code http-code))
      (etypecase unparsed-response
        (string unparsed-response)
        (vector (babel:octets-to-string unparsed-response))))))

(defun handle-response (response request-id)
  "Handle a JSON method call response, or a list of those."
  (let ((id (cdr (assoc :id response)))
        (error (cdr (assoc :error response)))
        (result (cdr (assoc :result response))))
    (cond ((and (null error) (null result))
           (mapcar #'handle-response response request-id))
          (t
           (unless (eql id request-id)
             (error 'jsonrpc-id-mismatch
                    :expected-id request-id
                    :actual-id id))
           (when error
             (error 'jsonrpc-method-error :info error))
           result))))
    
(defun jsonrpc-call (method &rest params)
  "Call the procedure designated by METHOD via JSONRPC over HTTP."
  (let ((id (make-id)))
    (handle-response
     (json:decode-json-from-string
      (request-over-http
       (json:encode-json-to-string
        (make-request method params id))))
     id)))

(defun jsonrpc-multicall (calls)
  "Multi-call the procedures designated by CALLS via JSONRPC over HTTP."
  (let ((ids (loop for call in calls collect (make-id))))
    (let ((requests (mapcar (lambda (call id) (make-request (car call) (cdr call) id)) calls ids)))
      (handle-response
       (json:decode-json-from-string
        (request-over-http
         (json:encode-json-to-string
          requests)))
       ids))))

(defun jsonrpc-set-siyobik ()
  (setf *jsonrpc-certificate* "/home/death/lisp/juarez/data/death.pem")
  (setf *jsonrpc-key* "/home/death/lisp/juarez/data/death.pem")
  (setf *jsonrpc-uri* "https://xxxxxxxxxxxx:6780/warehouse")
  (values))
