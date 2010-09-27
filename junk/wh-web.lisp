(eval-when (:compile-toplevel :load-toplevel :execute)
  (dolist (system '(#:juarez #:hunchentoot #:constantia #:yaclml
                    #:parenscript #:alexandria #:bordeaux-threads
                    #:sb-concurrency))
    (asdf:oos 'asdf:load-op system)))

(defpackage #:wh-web
  (:use #:cl)
  (:import-from #:hunchentoot
                #:create-static-file-dispatcher-and-handler
                #:create-prefix-dispatcher #:start #:stop #:acceptor
                #:*dispatch-table* #:get-parameter #:*acceptor*)
  (:import-from #:constantia #:outs)
  (:import-from #:juarez.notifications
                #:with-open-notification-client
                #:make-warehouse-notification-client
                #:download-torrent-by-id #:search-release-re
                #:add-notification-watcher #:notification-event-dispatch
                #:notification-type #:notification-content
                #:notification-client-method-error
                #:notification-client-error-message)
  (:import-from #:juarez.utils
                #:with-alist-values #:approximate-size)
  (:import-from #:yaclml
                #:with-yaclml-output-to-string)
  (:import-from #:ps
                #:ps #:@ #:new #:defpsmacro)
  (:import-from #:bt
                #:make-lock #:make-thread #:with-lock-held #:join-thread)
  (:import-from #:sb-concurrency
                #:make-mailbox #:receive-message-no-hang
                #:receive-message #:send-message)
  (:import-from #:alexandria
                #:assoc-value #:destructuring-case))

(in-package #:wh-web)

(defclass wh-server (acceptor)
  ((messages :initform '())
   (messages-lock :initform (make-lock "messages lock"))
   (client-inbox :initform (make-mailbox))
   (client-outbox :initform (make-mailbox))
   (client-thread)))

(defmethod initialize-instance :after ((server wh-server) &key)
  (with-slots (client-thread) server
    (setf client-thread (make-thread (client-thread-function server)))))

(defun client-thread-function (server)
  (with-slots (client-inbox client-outbox) server
    (lambda ()
      (block thread-function
        (loop
         (with-open-notification-client (client (make-warehouse-notification-client))
           (add-notification-watcher (lambda (notification)
                                       (add-message (notification-message notification) server))
                                     client)
           (handler-case
               (loop
                (handler-case
                    (progn
                      (notification-event-dispatch client)
                      (multiple-value-bind (message got-message)
                          (receive-message-no-hang client-inbox)
                        (when got-message
                          (destructuring-case message
                            ((:die)
                             (return-from thread-function))
                            ((:search query)
                             (send-message client-outbox (search-release-re client query)))
                            ((:queue site id)
                             (download-torrent-by-id client site id))))))
                  (notification-client-method-error (e)
                    (add-message (princ-to-string e) server))))
             (error (e)
               (add-message (princ-to-string e) server)))))))))
    
(defun notification-message (notification)
  (outs (:cc (notification-type notification)) " "
        (assoc-value (notification-content notification) :name)))

(defun client-command (server &rest command)
  (with-slots (client-inbox) server
    (send-message client-inbox command)))

(setf *dispatch-table*
      (list (create-static-file-dispatcher-and-handler "/style.css" "/home/death/lisp/juarez/data/wh-web-style.css" "text/css")
            (create-prefix-dispatcher "/search-results" 'search-results)
            (create-prefix-dispatcher "/queue-release" 'queue-release)
            (create-prefix-dispatcher "/periodic" 'periodic)
            (create-prefix-dispatcher "/" 'main)))

(defun start-server (&optional (port 8080))
  (start (make-instance 'wh-server :port port)))

(defun stop-server (&optional (server *acceptor*))
  (stop server)
  (with-slots (client-thread) server
    (when client-thread
      (client-command server :die)
      (join-thread client-thread)
      (setf client-thread nil))))

(defun add-message (string &optional (server *acceptor*))
  (with-slots (messages messages-lock) server
    (with-lock-held (messages-lock)
      (push string messages))))

(defun server-messages (&optional (server *acceptor*))
  (with-slots (messages messages-lock) server
    (with-lock-held (messages-lock)
      (reverse messages))))

(defun queue-release (&optional (server *acceptor*))
  (let ((id (parse-integer (get-parameter "id")))
        (site (get-parameter "site")))
    (add-message (outs "Queuing release " id " (" site ")") server)
    (client-command server :queue site id)))

(defun search-results (&optional (server *acceptor*))
  (with-slots (client-outbox) server
    (client-command server :search (get-parameter "q"))
    (search-results-html
     (receive-message client-outbox))))

(defun search-results-html (results)
  (with-yaclml-output-to-string
    (dolist (site-results results)
      (with-alist-values ((site results) site-results)
        (when results
          (<:table :class "search-results-table"
                   (<:caption (<:ah site))
                   (<:thead
                    (<:tr
                     (<:th "Name")
                     (<:th "Size")
                     (<:th "Queue")))
                   (<:tbody
                    (let ((odd t))
                      (dolist (result results)
                        (with-alist-values ((id name size) result)
                          (<:tr :class (if (setf odd (not odd)) "even" "odd")
                                (<:td (<:ah name))
                                (<:td (<:ah (destructuring-bind (n unit)
                                                (approximate-size size)
                                              (outs (:f n :digits-after-point 2) " " (:a unit)))))
                                (<:td (<:button :type "button" :onclick (outs "queueRelease('" site "', " id ")") "Queue")))))))))))))
                                                             
(defun periodic (&optional (server *acceptor*))
  (with-yaclml-output-to-string
    (<:p
     (dolist (message (server-messages server))
       (<:ah message)
       (<:br)))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defpsmacro with-ajax-request ((var url) &body forms)
    `(call-with-ajax-request ,url (lambda (,var) ,@forms))))

(defun main-js ()
  (ps (defun call-with-ajax-request (url fn)
        (let ((req (new *x-m-l-http-request)))
          (setf (@ req onreadystatechange)
                (lambda ()
                  (when (and (= (@ req ready-state) 4)
                             (= (@ req status) 200))
                    (funcall fn req))))
          ((@ req open) "GET" url t)
          ((@ req send))))
      (defun ajax-setup ()
        (set-interval #'ajax-request 1000))
      (defun ajax-request ()
        (with-ajax-request (req "periodic")
          (setf (@ ((@ document get-element-by-id) "periodic") inner-h-t-m-l)
                (@ req response-text))))
      (setf (@ window onload) #'ajax-setup)
      (defun perform-search ()
        (with-ajax-request (req (+ "search-results?q=" (@ ((@ document get-element-by-id) "search-query") value)))
          (setf (@ ((@ document get-element-by-id) "search-results") inner-h-t-m-l)
                (@ req response-text))))
      (defun queue-release (site id)
        (with-ajax-request (req (+ "queue-release?site=" site "&id=" id))
          (values)))))

(defun main ()
  (with-yaclml-output-to-string
    (<:html
     (<:head
      (<:title "Warehouse on ze Web")
      (<:meta :http-equiv "Content-Type" :content "text/html;charset=utf-8")
      (<:link :rel "stylesheet" :type "text/css" :href "style.css")
      (<:script :type "text/javascript" (<:ai (main-js))))
     (<:body
      (<:h1 "Warehouse on ze Web")
      (<:div :id "search"
             (<:input :id "search-query" :type "text")
             (<:button :type "button" :onclick "performSearch()" "Search")
             (<:div :id "search-results"))
      (<:div :id "periodic")))))
