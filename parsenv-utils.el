;;; parsenv-utils.el --- utils for parsenv
;; Copyright (C) 2017  Dan Harms (dharms)
;; Author: Dan Harms <enniomore@icloud.com>
;; Created: Monday, December  4, 2017
;; Version: 1.0
;; Modified Time-stamp: <2017-12-07 17:50:14 dharms>
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
;; Utilities for parsing .env files.
;;

;;; Code:
(require 'subr-x)

(defun parsenv-utils-read-file-into-list-of-lines (file)
  "Read FILE into a list of strings split line by line."
  (interactive "f")
  (with-temp-buffer
    (insert-file-contents file)
    (split-string (buffer-string) "\n" t)))

(defun parsenv-utils-strip-comments (line)
  "Strip comments from LINE."
  (while (string-match ".*?\\(#.*\\)$" line)
    (setq line (replace-match "" nil nil line 1)))
  line)

(defun parsenv-utils-continuation-p (line)
  "Return non-nil if LINE ends in a continuation character `\'."
  (string-match-p "\\\\$" line))

(defun parsenv-utils-strip-continuation (line)
  "Strip any trailing continuation character `\' from LINE."
  (if (string-match "\\\\$" line)
      (replace-match "" nil nil line)
    line))

(defun parsenv-utils-strip-export (line)
  "Strip any `export=' prefix from LINE."
  (if (string-match "^\\s-*export\\s-+" line)
      (replace-match "" nil nil line)
    line))

(defun parsenv-utils-transform-line (line)
  "Transform LINE by removing extraneous data."
  (thread-first line
    (parsenv-utils-strip-export)
    (string-trim)
    ))

(defun parsenv-utils-extract-key-value (line)
  "Extract any key=value pair present in LINE, as a cons (key . value).
In the absence of a `=', the key will be set to the string's
content, if any, and the value will be blank.  Note that we
assume whitespace from the ends has been trimmed."
  (if (string-match "^\\(.+?\\)=\\(.*\\)$" line)
      (cons
       (match-string-no-properties 1 line)
       (match-string-no-properties 2 line))
    (cons line "")))

(defun parsenv-utils-delimited-by-p (line char)
  "Return non-nil if LINE is delimited at both ends by CHAR.
The return value will be the string with the delimiters removed.
It is assumed that the input has already had whitespace trimmed."
  (let ((fmt (format "^%c\\(.*\\)%c$" char char)))
    (and (string-match fmt line)
         (match-string-no-properties 1 line))))


(defun parsenv-utils-consolidate-continuations (lst)
  "Return a list that is the result of removing continuations from LST.
Removed lines will be combined with the next element."
  (let (elt result)
    (while (setq elt (car lst))
      (when (parsenv-utils-continuation-p elt)
        (setq elt
              (concat (parsenv-utils-strip-continuation elt)
                      (cadr lst)))
        (setq lst (cdr lst)))
      (setq result (cons elt result))
      (setq lst (cdr lst)))
    (nreverse result)))

(defun parsenv-utils-transform-lines (lines)
  "Conduct the proper transforms across all elements of LINES."
  (let ((lst (mapcar 'parsenv-utils-strip-comments lines)))
    (mapcar 'parsenv-utils-transform-line lst)))

(provide 'parsenv-utils)
;;; parsenv-utils.el ends here
