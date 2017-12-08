;;; parsenv.el --- parse .env files
;; Copyright (C) 2017  Dan Harms (dharms)
;; Author: Dan Harms <enniomore@icloud.com>
;; Created: Monday, December  4, 2017
;; Version: 1.0
;; Modified Time-stamp: <2017-12-08 08:04:40 dharms>
;; Modified by: Dan Harms
;; Keywords: tools
;; URL: https://github.com/articuluxe/parsenv.git
;; Package-Requires: ((emacs "25"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; Parse .env files.
;;

;;; Code:
(require 'subr-x)

(defun parsenv-read-file-into-list-of-lines (file)
  "Read FILE into a list of strings split line by line."
  (interactive "f")
  (with-temp-buffer
    (insert-file-contents file)
    (split-string (buffer-string) "\n" t)))

(defun parsenv-strip-comments (line)
  "Strip comments from LINE."
  (while (string-match ".*?\\(#.*\\)$" line)
    (setq line (replace-match "" nil nil line 1)))
  line)

(defun parsenv-continuation-p (line)
  "Return non-nil if LINE ends in a continuation character `\'."
  (string-match-p "\\\\$" line))

(defun parsenv-strip-continuation (line)
  "Strip any trailing continuation character `\' from LINE."
  (if (string-match "\\\\$" line)
      (replace-match "" nil nil line)
    line))

(defun parsenv-strip-export (line)
  "Strip any `export=' prefix from LINE."
  (if (string-match "^\\s-*export\\s-+" line)
      (replace-match "" nil nil line)
    line))

(defun parsenv-transform-line (line)
  "Transform LINE by removing extraneous data."
  (thread-first line
    (parsenv-strip-export)
    (string-trim)
    ))

(defun parsenv-extract-key-value (line)
  "Extract any key=value pair present in LINE, as a cons (key . value).
In the absence of a `=', the key will be set to the string's
content, if any, and the value will be blank.  Note that we
assume whitespace from the ends has been trimmed.  Note also that
the value can be left blank even if `=' is present."
  (if (string-match "^\\(.+?\\)=\\(.*\\)$" line)
      (cons
       (match-string-no-properties 1 line)
       (match-string-no-properties 2 line))
    (cons line "")))

(defun parsenv-delimited-by-p (line char)
  "Return non-nil if LINE is delimited at both ends by CHAR.
The return value will be the string with the delimiters removed.
It is assumed that the input has already had whitespace trimmed."
  (let ((fmt (format "^%c\\(.*\\)%c$" char char)))
    (and (string-match fmt line)
         (match-string-no-properties 1 line))))


(defun parsenv-consolidate-continuations (lst)
  "Return a list that is the result of removing continuations from LST.
Removed lines will be combined with the next element."
  (let (elt result)
    (while (setq elt (car lst))
      (when (parsenv-continuation-p elt)
        (setq elt
              (concat (parsenv-strip-continuation elt)
                      (cadr lst)))
        (setq lst (cdr lst)))
      (setq result (cons elt result))
      (setq lst (cdr lst)))
    (nreverse result)))

(defun parsenv-transform-lines (lines)
  "Conduct the proper transforms across all elements of LINES."
  (let ((lst (mapcar 'parsenv-strip-comments lines)))
    (mapcar 'parsenv-transform-line lst)))

(defun parsenv-load-env-vars-from-file (file)
  "Load each line from FILE, of the form `var=val'.
For each line, sets environment variable `var' equal to `val'."
  (interactive "fLoad environment variables from file: ")
  (mapc (lambda(line)
          (when (string-match "\\(.+\\)=\\(.*\\)" line)
            (setenv (match-string-no-properties 1 line)
                    (substitute-env-vars
                     (match-string-no-properties 2 line)))))
        (parsenv-read-file-into-list-of-lines file)))

(defun parsenv-load-env-var-from-file (var file &optional sep)
  "Load into the environment variable VAR each line from FILE.
Existing values will be maintained.  SEP is an optional separator."
  (interactive)
  (unless sep (setq sep path-separator))
  (setenv var (concat (mapconcat 'convert-standard-filename
                                 (parsenv-read-file-into-list-of-lines file)
                                 sep) sep (getenv var))))

(provide 'parsenv)
;;; parsenv.el ends here
