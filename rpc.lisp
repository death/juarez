;;;; +----------------------------------------------------------------+
;;;; | JUAREZ - Warez tools                               DEATH, 2010 |
;;;; +----------------------------------------------------------------+

(in-package #:juarez.rpc)


;;;; JSON RPC API to qto's warehouse

(defvar *json-rpc-id-counter* 0)

(defun json-rpc-call (method-name &rest params)
  (incf *json-rpc-id-counter*)
  (json:encode-json-to-string
   `((method . ,method-name)
     (id . ,*json-rpc-id-counter*)
     (jsonrpc . "2.0")
     (params . ,(if (null params)
                    #()
                    params)))))

(defun siyobik-http-request (content &key (method :post) (test nil))
  (let ((drakma::*ssl-certificate* "/home/death/lisp/juarez/data/death.pem")
        (drakma::*ssl-key* "/home/death/lisp/juarez/data/death.pem"))
    (drakma:http-request
     (if test
         "https://xxxxxxxxxxxx:6780/"
         "https://xxxxxxxxxxxx:6780/warehouse")
     :method method
     :content content)))

(defun siyobik-call (method-name &rest params)
  (multiple-value-bind (response-text code)
      (siyobik-http-request (apply #'json-rpc-call method-name params))
    (unless (eql code 200)
      (error "Failed siyobik call: ~A." code))
    (unless (stringp response-text)
      (setf response-text (babel:octets-to-string response-text)))
    (json:json-bind (result error id) response-text
      (when (not (eql id *json-rpc-id-counter*))
        (error "Bad id: ~A expected: ~A" id *json-rpc-id-counter*))
      (when error
        (error "Method error: ~A" error))
      result)))
