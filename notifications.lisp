;;;; +----------------------------------------------------------------+
;;;; | JUAREZ - Warez tools                               DEATH, 2010 |
;;;; +----------------------------------------------------------------+

(in-package #:juarez.notifications)


;;; Notification Client protocol

(defclass notification-client ()
  ())

(defgeneric notification-client-host (notification-client))
(defgeneric notification-client-port (notification-client))
(defgeneric notification-client-ssl-key (notification-client))
(defgeneric notification-client-ssl-certificate (notification-client))

(defgeneric open-notification-client (notification-client))
(defgeneric close-notification-client (notification-client))

(defgeneric add-notification-watcher (watcher notification-client))
(defgeneric remove-notification-watcher (watcher notification-client))

(defgeneric notification-event-dispatch (notification-client &key wait))
(defgeneric dispatch-notification (notification notification-client))

(defgeneric notification-client-rpc (id method-json notification-client))
(defgeneric grab-notification-client-rpc-id (notification-client))

(defmacro with-open-notification-client ((var notification-client) &body forms)
  `(let ((,var ,notification-client))
     (open-notification-client ,var)
     (unwind-protect (progn ,@forms)
       (when ,var
         (close-notification-client ,var)))))


;;;; Basic notification client
     
(defclass basic-notification-client (notification-client)
  ((host :initarg :host :reader notification-client-host)
   (port :initarg :port :reader notification-client-port)
   (ssl-key :initarg :ssl-key :reader notification-client-ssl-key)
   (ssl-certificate :initarg :ssl-certificate :reader notification-client-ssl-certificate)
   (connection :initform nil :accessor notification-client-connection)
   (watchers :initform '() :accessor notification-client-watchers)
   (last-id :initform 0 :accessor notification-client-last-id)))

(defmethod open-notification-client ((client basic-notification-client))
  (let ((socket (make-socket)))
    (unwind-protect
         (progn
           (connect socket (lookup-hostname (notification-client-host client))
                    :port (notification-client-port client))
           (setf (notification-client-connection client)
                 (make-ssl-client-stream (fd-of socket)
                                         :key (notification-client-ssl-key client)
                                         :certificate (notification-client-ssl-certificate client)
                                         :close-callback (let ((socket socket))
                                                           (lambda () (close socket)))))
           (setf socket nil)
           client)
      (when socket
        (close socket)))))

(defmethod close-notification-client ((client basic-notification-client))
  (when (notification-client-connection client)
    (close (notification-client-connection client))
    (setf (notification-client-connection client) nil)))

(defmethod add-notification-watcher ((watcher function) (client basic-notification-client))
  (pushnew watcher (notification-client-watchers client)))

(defmethod remove-notification-watcher ((watcher function) (client basic-notification-client))
  (deletef (notification-client-watchers client) watcher))

(defmethod notification-event-dispatch ((client basic-notification-client) &key wait)
  (when (or wait (listen (notification-client-connection client)))
    (dispatch-notification (wait-for-notification client) client)))

(defun wait-for-notification (client)
  (loop for message = (read-message (notification-client-connection client))
        until (notification-message-p message)
        finally (return message)))

(defun wait-for-reply (id client)
  (loop for message = (read-message (notification-client-connection client))
        until (reply-with-id-p id message)
        when (notification-message-p message)
        do (dispatch-notification message client)
        finally (return message)))

(defun notification-message-p (message)
  (typep message 'notification-message))

(defun reply-with-id-p (id message)
  (or (typep message 'error-message)
      (and (typep message 'result-message)
           (= id (message-id message)))))       

(defmethod dispatch-notification (notification (client basic-notification-client))
  (dolist (watcher (notification-client-watchers client))
    (funcall watcher notification)))

(defun read-message (stream)
  (flet ((consume () (read-byte stream)))
    (let ((size 0))
      (tagbody
       read-size
         (let ((octet (consume)))
           (case octet
             ((#x30 #x31 #x32 #x33 #x34 #x35 #x36 #x37 #x38 #x39)
              (setf size (+ (* size 10) (- octet #x30))))
             (#x3A (go read-payload))))
         (go read-size)
       read-payload
         (let ((payload (make-array size :element-type '(unsigned-byte 8))))
           (read-sequence payload stream)
           (return-from read-message
             (parse-message (decode-message payload))))))))

(defun decode-message (octets)
  (decode-json-from-string
   (octets-to-string octets :encoding :utf-8)))

(defvar *message-parsers*
  (make-hash-table :test 'equal))

(defun find-message-parser (message-type)
  (gethash message-type *message-parsers*))

(defun (setf find-message-parser) (message-parser message-type)
  (setf (gethash message-type *message-parsers*) message-parser))

(defclass message ()
  ())

(defclass generic-message (message)
  ((json :initarg :json :reader message-json)))

(defun parse-message (message-json)
  (let ((type (assoc-value message-json :type)))
    (if-let (parser (find-message-parser type))
      (funcall parser (assoc-value message-json :data))
      (make-instance 'generic-message :json message-json))))

(defclass result-message (message)
  ((id :initarg :id :reader message-id)
   (error :initarg :error :reader message-error)
   (result :initarg :result :reader message-result)))

(setf (find-message-parser "rpcResult")
      (lambda (json)
        (with-alist-values ((id error result) json)
          (make-instance 'result-message :id id :error error :result result))))

(defclass error-message (message)
  ((error :initarg :error :reader message-error)))

(setf (find-message-parser "error")
      (lambda (error)
        (make-instance 'error-message :error error)))

(defclass notification-message (message)
  ((time :initarg :time :reader notification-time)
   (type :initarg :type :reader notification-type)
   (content :initarg :content :reader notification-content)))

(setf (find-message-parser "notification")
      (lambda (json)
        (with-alist-values ((time type content) json)
          (make-instance 'notification-message :time time :type type :content content))))

(defun encode-json-to-message (json)
  (let ((string (encode-json-to-string json)))
    (string-to-octets
     (format nil "~D:~A" (string-size-in-octets string :encoding :utf-8) string)
     :encoding :utf-8)))

(defgeneric interpret-reply (message))

(define-condition notification-client-error (error)
  ((message :initarg :message :reader notification-client-error-message))
  (:report report-notification-client-error))

(defun report-notification-client-error (error stream)
  (format stream "Notification client error: ~S." (notification-client-error-message error)))

(defmethod interpret-reply ((message error-message))
  (error 'notification-client-error :message (message-error message)))

(define-condition notification-client-method-error (notification-client-error)
  ())

(defmethod interpret-reply ((message result-message))
  (when (message-error message)
    (error 'notification-client-method-error :message (message-error message)))
  (message-result message))

(defmethod notification-client-rpc (id method-json (client basic-notification-client))
  (write-sequence (encode-json-to-message method-json) (notification-client-connection client))
  (force-output (notification-client-connection client))
  (interpret-reply (wait-for-reply id client)))

(defmethod grab-notification-client-rpc-id ((client basic-notification-client))
  (incf (notification-client-last-id client)))


;;;; RPC methods for notifications

(defmacro define-rpc-method (lisp-name arglist original-name)
  (with-gensyms (client id)
    `(defun ,lisp-name (,client ,@arglist)
       (let ((,id (grab-notification-client-rpc-id ,client)))
         (notification-client-rpc
          ,id
          (list (cons 'type "rpc")
                (cons 'data
                      (list (cons 'id ,id)
                            (cons 'method ,original-name)
                            (cons 'params (vector ,@arglist)))))
          ,client)))))

(define-rpc-method get-new-notifications () "getNewNotifications")
(define-rpc-method get-notifications-count () "getNotificationCount")
(define-rpc-method get-old-notifications (start end) "getOldNotifications")
(define-rpc-method generate-notification (type content) "generateNotification")
(define-rpc-method get-torrents () "getTorrents")
(define-rpc-method download-torrent-by-id (site id) "downloadTorrentById")
(define-rpc-method get-site-statistics (site) "getSiteStatistics")
(define-rpc-method search-release (query) "search")
(define-rpc-method search-release-re (query) "regexSearch")


;;;; Warehouse notification client

(defun make-warehouse-notification-client ()
  (make-instance 'basic-notification-client
                 :host "xxxxxxxxxxxx"
                 :port 43841
                 :ssl-key "/home/death/lisp/juarez/data/death.pem"
                 :ssl-certificate "/home/death/lisp/juarez/data/death.pem"))
