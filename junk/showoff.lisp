(in-package :juarez)

(defstruct search-result
  site
  name
  section
  size
  date
  id)

(defparameter *columns*
  '(("Name" search-result-name)
    ("Site" search-result-site)
    ("Section" search-result-section)
    ("Size" search-result-size)
    ("Date" search-result-date)
    ("ID" search-result-id)))

(defun warehouse-search (text)
  (let ((search-results '()))
    (dolist (site-results (get-site-results text))
      (with-alist-values ((site results) site-results)
        (dolist (result results)
          (with-alist-values ((name section date id size) result)
            (push (make-search-result
                   :site site
                   :name name
                   :section section
                   :size (princ-to-string size)
                   :date (princ-to-string date)
                   :id (princ-to-string id))
                  search-results)))))
    (nreverse search-results)))

(defun get-site-results (query)
  (with-open-notification-client (client (make-warehouse-notification-client))
    (search-release-re client query)))

(defun store-remove-all-items (store)
  (with-slots ((items gtk::items)) store
    (do ((n (1- (length items)) (1- n)))
        ((< n 0))
      (vector-pop items)
      (let ((path (make-instance 'gtk:tree-path)))
        (setf (gtk:tree-path-indices path) (list n))
        (gobject:emit-signal store "row-deleted" path)))))      

(defun run ()
  (gtk:within-main-loop
    (let ((window (make-instance 'gtk:gtk-window
                                 :type :toplevel
                                 :window-position :center
                                 :title "Hello world"
                                 :default-width 500
                                 :default-height 500))
          (model (make-instance 'gtk:array-list-store))
          (scroll (make-instance 'gtk:scrolled-window :hscrollbar-policy :automatic :vscrollbar-policy :automatic))
          (tv (make-instance 'gtk:tree-view :headers-visible t :width-request 450 :height-request 400 :rules-hint t))
          (button (make-instance 'gtk:button :label "Search"))
          (vbox (make-instance 'gtk:v-box))
          (hbox (make-instance 'gtk:h-box))
          (search-entry (make-instance 'gtk:entry)))
      (loop for (name getter) in *columns*
            do (gtk:store-add-column model "gchararray" (fdefinition getter)))
      (gobject:connect-signal button "clicked"
                              (lambda (b)
                                (declare (ignore b))
                                (store-remove-all-items model)
                                (loop for result in (warehouse-search (gtk:entry-text search-entry))
                                      do (gtk:store-add-item model result))))
      (setf (gtk:tree-view-model tv) model)
      (setf (gtk:tree-view-tooltip-column tv) 0)
      (gobject:g-signal-connect window "delete-event"
                                (lambda (window event)
                                  (declare (ignore window event))
                                  (gtk:gtk-main-quit)))
      (gtk:container-add window vbox)
      (gtk:box-pack-start vbox hbox :expand nil)
      (gtk:box-pack-start hbox search-entry :expand t)
      (gtk:box-pack-start hbox button :expand nil)
      (gtk:box-pack-start vbox scroll)
      (gtk:container-add scroll tv)
      (loop for (name getter) in *columns*
            for colnum from 0
            for column = (make-instance 'gtk:tree-view-column :title name)
            for renderer = (make-instance 'gtk:cell-renderer-text :text "A text") do
            (gtk:tree-view-column-pack-start column renderer)
            (gtk:tree-view-column-add-attribute column renderer "text" colnum)
            (gtk:tree-view-append-column tv column))
      (gtk:widget-show window))))
