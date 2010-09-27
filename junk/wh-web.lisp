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
                #:*yaclml-indent*
                #:with-yaclml-output-to-string)
  (:import-from #:ps
                #:ps #:@ #:new #:defpsmacro)
  (:import-from #:bt
                #:make-thread #:join-thread)
  (:import-from #:sb-concurrency
                #:make-mailbox #:receive-message-no-hang
                #:receive-message #:receive-pending-messages #:send-message)
  (:import-from #:alexandria
                #:assoc-value #:destructuring-case #:when-let #:appendf))

(in-package #:wh-web)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf *yaclml-indent* nil))

(defclass wh-server (acceptor)
  ((client-inbox :initform (make-mailbox))
   (client-outbox-search-results :initform (make-mailbox))
   (client-outbox-notifications :initform (make-mailbox))
   (client-thread)
   (all-messages :initform '())))

(defmethod initialize-instance :after ((server wh-server) &key)
  (with-slots (client-thread) server
    (setf client-thread (make-thread (client-thread-function server)))))

(defun client-thread-function (server)
  (with-slots (client-inbox client-outbox-search-results client-outbox-notifications) server
    (lambda ()
      (block thread-function
        (loop
         (with-open-notification-client (client (make-warehouse-notification-client))
           (add-notification-watcher (lambda (notification)
                                       (send-message client-outbox-notifications
                                                     (notification-message notification)))
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
                             (send-message client-outbox-search-results (search-release-re client query)))
                            ((:queue site id)
                             (download-torrent-by-id client site id))))))
                  (notification-client-method-error (e)
                    (send-message client-outbox-notifications (princ-to-string e)))))
             (error (e)
               (send-message client-outbox-notifications (princ-to-string e))))))))))
    
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

(defun queue-release (&optional (server *acceptor*))
  (let ((id (parse-integer (get-parameter "id")))
        (site (get-parameter "site")))
    (add-message (outs "Queuing release " id " (" site ")") server)
    (client-command server :queue site id)))

(defun search-results (&optional (server *acceptor*))
  (with-slots (client-outbox-search-results) server
    (client-command server :search (get-parameter "q"))
    (search-results-html
     (receive-message client-outbox-search-results))))

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
  (with-slots (client-outbox-notifications all-messages) server
    (when-let (pending (receive-pending-messages client-outbox-notifications))
      (appendf all-messages pending))
    (with-yaclml-output-to-string
      (<:p
       (dolist (message all-messages)
         (<:ah message)
         (<:br))))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defpsmacro with-ajax-request ((var base-url &rest args) &body forms)
    `(call-with-ajax-request
      ,(cond ((null args) base-url)
             (t (list* '+ base-url
                       (loop for prefix = "?" then "&"
                             for (name val-form) on args by #'cddr
                             collect (outs prefix name "=")
                             collect val-form))))
      (lambda (,var) ,@forms))))

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
      (defun get-document-element (id)
        ((@ document get-element-by-id) id))
      (defun ajax-setup ()
        (set-interval #'ajax-request 1000))
      (defun ajax-request ()
        (with-ajax-request (req "periodic")
          (setf (@ (get-document-element "periodic") inner-h-t-m-l)
                (@ req response-text))))
      (defun perform-search ()
        (with-ajax-request (req "search-results" "q" (@ (get-document-element "search-query") value))
          (setf (@ (get-document-element "search-results") inner-h-t-m-l)
                (@ req response-text))))
      (defun queue-release (site id)
        (with-ajax-request (req "queue-release" "site" site "id" id)
          (values)))
      (setf (@ window onload) #'ajax-setup)))

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
