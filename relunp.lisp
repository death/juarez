;;;; +----------------------------------------------------------------+
;;;; | JUAREZ - Warez tools                               DEATH, 2010 |
;;;; +----------------------------------------------------------------+

;;;; A semi-intelligent release unpacker
;;
;;   Scan a directory and its subdirectories.  For each filename,
;;   dispatch on type:
;;
;;            .-----------.--------------------------.
;;            | Type      | Action                   |
;;            |-----------+--------------------------|
;;            | directory | Add to DIRECTORIES list  |
;;            | AVI       | Add to MOVIES list       |
;;            | SUB       | Add to SUBTITLES list    |
;;            | RAR       | Add to UNPACKABLE list   |
;;            | NFO       | Add to DELETABLE list    |
;;            | IDX       | Ignore (goes with SUB)   |
;;            | Rdd       | Ignore (goes with RAR)   |
;;            | Other     | Ignore                   |
;;            '-----------'--------------------------'
;;
;;   Now we can:
;;
;;   - Unpack files in UNPACKABLE list and add them (and their friends)
;;     to DELETABLE list.  (UNPACK, UNPACKABLE-FRIENDS)
;;
;;   - Ensure files in SUBTITLES list (and their friends) are in the
;;     same directory as their corresponding friends in the MOVIES list.
;;     (CORRESPONDING-MOVIE, SUBTITLE-FRIENDS, RENAME-FILE)
;;
;;   - Add empty directories in the DIRECTORIES list to DELETEABLE
;;     list.  (EMPTY-DIRECTORY-P)    
;;
;;   - Delete all the files and directories in the DELETABLE list.
;;     (DELETE-DIRECTORY-AND-FILES)
;;

(in-package #:juarez)


;;;; Utilities

(defun empty-directory-p (pathname)
  (and (fad:directory-pathname-p pathname)
       (null (fad:list-directory pathname))))

(defun collect-friends (predicate pathname)
  (loop for file in (directory (make-pathname :type :wild :defaults pathname))
        when (funcall predicate (pathname-type file))
        collect file))

(defun just-directory (pathname)
  (make-pathname :name nil :type nil :defaults pathname))

(defun call-with-current-directory (function pathname)
  (let ((saved-directory (sb-posix:getcwd)))
    (unwind-protect
         (progn
           (sb-posix:chdir (namestring (just-directory pathname)))
           (funcall function))
      (sb-posix:chdir saved-directory))))

(defmacro with-current-directory ((pathname) &body forms)
  `(call-with-current-directory (lambda () ,@forms) ,pathname))

(defun move-to-directory (target pathnames)
  (dolist (pathname pathnames)
    (let ((new-pathname (make-pathname :name (pathname-name pathname)
                                       :type (pathname-type pathname)
                                       :defaults target)))
      (rename-file pathname new-pathname))))

(defun same-directory-p (pathname1 pathname2)
  (equalp (just-directory pathname1) (just-directory pathname2)))

(defun delete-whatever (pathname)
  (cond ((fad:directory-pathname-p pathname)
         (fad:delete-directory-and-files pathname :if-does-not-exist :ignore))
        ((fad:file-exists-p pathname)
         (delete-file pathname))))


;;;; Pathname blacklisting

(defvar *blacklist* '())

(defun blacklisted-p (pathname)
  (member pathname *blacklist* :test #'equalp))

(defun blacklist (pathname)
  (pushnew pathname *blacklist* :test #'equalp))

(defun clear-blacklist ()
  (setf *blacklist* '()))

(defun delete-blacklisted ()
  (dolist (pathname *blacklist*)
    (delete-whatever pathname))
  (clear-blacklist))


;;;; Workaround for braindead parts file naming convention

;;; Some stupid groups use file.partXX.rar instead of file.rXX

(defun non-first-part-p (pathname)
  (when (equalp (pathname-type pathname) "rar")
    (when-let (pos (search ".part" (pathname-name pathname)))
      (let ((n (parse-integer (pathname-name pathname)
                              :start (+ pos 5)
                              :end (+ pos 7)
                              :junk-allowed t)))
        (and (integerp n)
             (> n 1))))))


;;;; Classification

(defparameter *pathname-schema*
  '((blacklisted-p . blacklist)
    (empty-directory-p . deletable)
    ("avi" . movies)
    ("sub" . subtitles)
    (non-first-part-p . deletable)
    ("rar" . unpackable)
    (("nfo" "sfv") . deletable)))

(defun match-pathname (pathname pattern)
  (etypecase pattern
    ((or symbol function)
     (funcall pattern pathname))
    (string
     (equalp (pathname-type pathname) pattern))
    (list
     (some (lambda (pattern) (match-pathname pathname pattern)) pattern))))

(defun directory-contents (directory &key (schema *pathname-schema*) (contents (make-hash-table)))
  (fad:walk-directory directory
                      (lambda (pathname)
                        (when-let (entry (assoc pathname schema :test #'match-pathname))
                          (add-pathname pathname (cdr entry) contents)))
                      :directories :breadth-first)
  contents)

(defun add-pathname (pathname category contents)
  (push pathname (gethash category contents '())))


;;;; Category functions

(defun category-function (category)
  (get category 'process))

(defun (setf category-function) (new-value category)
  (setf (get category 'process) new-value))

(defmacro define-category-function (category (pathname contents) &body forms)
  `(progn
     (setf (category-function ',category)
           (lambda (,pathname ,contents)
             ,@forms))
     ',category))


;;;; Scanning

(defvar *rescan*)

(defun ensure-rescan ()
  (setf *rescan* t))

(defun relunp (directory &optional (*standard-output* *standard-output*))
  (let ((*rescan* t))
    (loop while *rescan* do
          (setf *rescan* nil)
          (process-contents (directory-contents directory)))))

(defun process-contents (contents)
  (dolist (category '(unpackable subtitles deletable))
    (dolist (pathname (gethash category contents))
      (funcall (category-function category) pathname contents))
    (remhash category contents)))


;;;; Unpackable category

(defun unpackable-friends (pathname)
  (collect-friends (lambda (type)
                     (and (stringp type)
                          (char-match '((#\r #\R) :digit :digit) type)))
                   pathname))

(defun unpack (pathname)
  (with-current-directory (pathname)
    (= (sb-ext:process-exit-code
        (sb-ext:run-program "/usr/bin/unrar"
                            (list "x" "-y" (namestring pathname))))
       0)))

(define-category-function unpackable (pathname contents)
  (out "Unpacking " pathname (:%))
  (cond ((unpack pathname)
         (add-pathname pathname 'deletable contents)
         (dolist (friend (unpackable-friends pathname))
           (add-pathname friend 'deletable contents))
         (ensure-rescan))
        (t (out "Could not unpack " pathname "." (:%)))))


;;;; Subtitles category

(defun corresponding-movie (pathname contents)
  (find (pathname-name pathname)
        (gethash 'movies contents)
        :test #'equal :key #'pathname-name))

(defun subtitle-friends (pathname)
  (collect-friends (lambda (type) (equalp type "idx")) pathname))

(define-category-function subtitles (pathname contents)
  (let ((movie (corresponding-movie pathname contents)))
    (cond ((and movie (not (same-directory-p pathname movie)))
           (out "Moving " pathname " to " (just-directory movie) (:%))
           (handler-case
               (move-to-directory movie (cons pathname (subtitle-friends pathname)))
             (error (e)
               (out "Could not move " pathname ":" (:%) e (:%))))
           (ensure-rescan))
          ((null movie)
           (warn "Subtitles without a movie: ~S." pathname)))))


;;;; Deletable category

(defvar *delete-action* :blacklist)

(define-category-function deletable (pathname contents)
  (declare (ignore contents))
  (out "Deleting " pathname (:%))
  (handler-case
      (ecase *delete-action*
        (:blacklist
         (blacklist pathname))
        (:delete
         (cond ((fad:directory-pathname-p pathname)
                (fad:delete-directory-and-files pathname))
               (t
                (delete-file pathname)
                (ensure-rescan)))))
    (error (e)
      (out "Could not delete " pathname ":" (:%) e (:%)))))

