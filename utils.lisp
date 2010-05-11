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

(defmacro with-alist-values ((keys alist) &body forms)
  (once-only (alist)
    `(let ,(loop for key in keys
                 collect `(,key (cdr (assoc ',key ,alist))))
       ,@forms)))
