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
  (let ((socket nil))
    (unwind-protect
         (progn
           (setf socket (socket-connect (notification-client-host client)
                                        (notification-client-port client)
                                        :element-type '(unsigned-byte 8)))
           (setf (notification-client-connection client)
                 (make-ssl-client-stream (socket-stream socket)
                                         :key (notification-client-ssl-key client)
                                         :certificate (notification-client-ssl-certificate client)
                                         :close-callback (let ((socket socket))
                                                           (lambda ()
                                                             (socket-close socket)))))
           (setf socket nil)
           client)
      (when socket
        (socket-close socket)))))

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
  (parse-message
   (decode-message
    (read-n-octets (read-size stream) stream))))

(defun read-size (stream)
  (let ((size 0))
    (loop for octet = (read-byte stream)
          until (= octet #x3A)
          when (<= #x30 octet #x39)
          do (setf size (+ (* size 10) (- octet #x30))))
    size))

(defun read-n-octets (n stream)
  (let ((octets (make-array n :element-type '(unsigned-byte 8))))
    (read-sequence octets stream)
    octets))

(defun decode-message (octets)
  (decode-json-from-string
   (octets-to-string octets :encoding :utf-8)))

(defvar *message-parsers*
  (make-hash-table :test 'equal))

(defun find-message-parser (message-type)
  (or (gethash message-type *message-parsers*)
      (gethash :generic *message-parsers*)))

(defun (setf find-message-parser) (message-parser message-type)
  (setf (gethash message-type *message-parsers*) message-parser))

(defclass message ()
  ())

(defun parse-message (message-json)
  (with-alist-values ((type data) message-json)
    (funcall (find-message-parser type) data)))

(defmacro define-message (message-type (&rest slot-names) class-name
                          &key (reader-conc-name "MESSAGE-"))
  (multiple-value-bind (vars json-var) (parse-varlist slot-names)
    (let ((initargs (mapcar #'make-keyword vars))
          (readers (loop for var in vars
                         collect (format-symbol t "~A~A" reader-conc-name var))))
      `(progn
         (defclass ,class-name (message)
           ,(mapcar (lambda (var initarg reader)
                      (list var :initarg initarg :reader reader))
                    vars initargs readers))
         (setf (find-message-parser ,message-type)
               (lambda (,json-var)
                 (with-alist-values (,(remove json-var vars) ,json-var)
                   (make-instance ',class-name
                                  ,@(mapcan #'list initargs vars)))))))))

(eval-always
  (defun parse-varlist (varlist)
    (let ((json-var (gensym)))
      (values (loop while varlist
                    collect (let ((x (pop varlist)))
                              (if (eq x '&json)
                                  (setf json-var (pop varlist))
                                  x)))
              json-var))))

(define-message :generic (&json json) generic-message)
(define-message "rpcResult" (id error result) result-message)
(define-message "error" (&json error) error-message)
(define-message "notification" (time type content) notification-message
                :reader-conc-name "NOTIFICATION-")

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
  (let ((client (make-symbol "CLIENT"))
        (id (gensym "ID")))
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

(define-rpc-method get-notifications (offset count) "getNotifications")
(define-rpc-method get-notifications-count () "getNotificationCount")
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
