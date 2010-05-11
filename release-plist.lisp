;;;; +----------------------------------------------------------------+
;;;; | JUAREZ - Warez tools                               DEATH, 2010 |
;;;; +----------------------------------------------------------------+

(in-package #:juarez)


;;;; Release strings to a parsed property list

(defparameter *properties*
  '("720p" "1080p" "BD5" "BD9" "PPV" "STV" "HDTV" "BluRay"
    "REPACK" "PROPER" "DTS" "SUBFiX" "LiMiTED" "x264" "WEB"
    "XXX" "DVDRiP" "XviD" "EP" "LP" "NTSC" "COMPLETE" "DVDR"
    "Cracked" "WS" "PDTV" "Remastered" "PAL" "x86" "x64"))

(defun release-plist (release-string)
  (when-let (words (split-release-string release-string))
    (let ((plist '()))
      (macrolet ((field (name word &key (translate 'identity) (eliminate 'remove))
                   (once-only (word)
                     `(when ,word
                        (setf words (,eliminate ,word words))
                        (push (,translate ,word) plist)
                        (push ',name plist)))))
        (field :group (lastcar words))
        (field :properties (just-properties words) :eliminate remove-properties :translate keywordify-words)
        (field :year (find-if #'year-word-p words :from-end t) :translate parse-integer)
        (field :season-and-episode (find-if #'season-and-episode-word-p words)
               :translate parse-season-and-episode)
        (field :title words :translate format-title))
      plist)))

(defun split-release-string (string)
  (split-sequence-if (lambda (char) (find char "_.-")) string :remove-empty-subseqs t))

(defun just-properties (words)
  (intersection *properties* words :test #'equalp))

(defun remove-properties (properties words)
  (remove-if (lambda (word) (member word properties :test #'equalp)) words))

(defun parse-season-and-episode (season-and-episode)
  (list (parse-integer season-and-episode :start 1 :end 3)
        (parse-integer season-and-episode :start 4 :end 6)))

(defun format-title (words)
  (format nil "~{~A~^ ~}" words))

(defun year-word-p (word)
  (char-match '((#\1 #\2) :digit :digit :digit) word))

(defun season-and-episode-word-p (word)
  (char-match '(#\S :digit :digit #\E :digit :digit) word))
