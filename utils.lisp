;;;; +----------------------------------------------------------------+
;;;; | JUAREZ - Warez tools                               DEATH, 2010 |
;;;; +----------------------------------------------------------------+

(in-package #:juarez)


;;;; General-purpose utilities

(defun keywordify-words (words)
  (mapcar #'keywordify words))

(defun keywordify (word)
  (intern (string-upcase word) :keyword))

(defun char-match (pattern string &optional (whole-string t))
  (with-input-from-string (in string)
    (and (every (lambda (x)
                  (let ((c (read-char in nil nil)))
                    (cond ((null c) nil)
                          ((characterp x) (char= x c))
                          ((eq :digit x) (digit-char-p c))
                          ((consp x) (member c x))
                          (t (error "Unknown pattern atom ~S." x)))))
                pattern)
         (or (not whole-string)
             (null (read-char in nil nil))))))

(defmacro with-alist-values ((keys alist &key keywords) &body forms)
  (once-only (alist)
    `(let ,(loop for key in keys
                 collect `(,key (cdr (assoc ',(if keywords
                                                  (as-keyword key)
                                                  key)
                                            ,alist))))
       ,@forms)))

(defparameter *size-units*
  '(tb gb mb kb bytes))

(defun approximate-size (bytes)
  (loop for unit in *size-units*
        for one = (expt 1024 (1- (length *size-units*))) then (/ one 1024)
        when (>= bytes one)
        return (list (let ((x (/ bytes one)))
                       (if (integerp x)
                           x
                           (float x)))
                     unit)))

(defun format-date (date &optional (stream *standard-output*))
  (destructuring-bind (&optional (year 2010) (month 01) (day 01) (hour 00) (minute 00) (second 00)) date
    (out (:to stream)
         (:d year :width 4 :pad-char #\0) "-"
         (:d month :width 2 :pad-char #\0) "-"
         (:d day :width 2 :pad-char #\0) "T"
         (:d hour :width 2 :pad-char #\0) ":"
         (:d minute :width 2 :pad-char #\0) ":"
         (:d second :width 2 :pad-char #\0))))
