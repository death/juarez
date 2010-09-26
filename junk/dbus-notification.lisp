(in-package #:juarez)

(defun show-notification (notification)
  (when (member (notification-type notification) '("queued" "downloaded") :test #'equal)
    (dbus:with-open-bus (bus (dbus:session-server-addresses))
      (dbus:with-introspected-object (notifications bus "/org/freedesktop/Notifications" "org.freedesktop.Notifications")
        (notifications "org.freedesktop.Notifications" "Notify"
                       "lul" 0 "/home/death/lisp/juarez/data/BigEyes-Creature-icon.png" "You've Got Warez"
                       (format nil "~A ~A"
                               (assoc-value (notification-content notification) :name)
                               (notification-type notification))
                       '() '() -1)))))

(defun start ()
  (with-open-notification-client (client (make-warehouse-notification-client))
    (add-notification-watcher #'show-notification client)
    (print 'ready)
    (loop (notification-event-dispatch client))))
