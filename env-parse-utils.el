;;; env-parse-utils.el --- utils for env-parse
;; Copyright (C) 2017  Dan Harms (dharms)
;; Author: Dan Harms <enniomore@icloud.com>
;; Created: Monday, December  4, 2017
;; Version: 1.0
;; Modified Time-stamp: <2017-12-04 18:03:55 dharms>
;; Modified by: Dan Harms
;; Keywords: tools
;; URL: https://github.com/articuluxe/env-parse.git
;; Package-Requires: ((emacs "24.4"))

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

(defun env-parse-utils-read-file-into-list-of-lines (file)
  "Read FILE into a list of strings split line by line."
  (interactive "f")
  (with-temp-buffer
    (insert-file-contents file)
    (split-string (buffer-string) "\n" t)))

(defun env-parse-utils-strip-comments (line)
  "Strip comments from LINE."
  (while (string-match ".*?\\(#.*\\)$" line)
    (setq line (replace-match "" nil nil line 1)))
  line)

(defun env-parse-utils-trim (line)
  "Trim whitespace from beginning and end of LINE."
  (string-trim line))

(defun env-parse-utils-continuation-p (line)
  "Return non-nil if LINE ends in a continuation character `\'."
  (string-match-p "\\$" line))

(defun env-parse-utils-strip-continuation (line)
  "Strip any trailing continuation character `\' from LINE."
  (when (string-match "\\$" line)
    (replace-match "" nil nil line 0)))






(defun env-parse-utils-load-env-vars-from-file (file)
  "Load each line from FILE, of the form `var=val'.
For each line, sets environment variable `var' equal to `val'."
  (interactive "fLoad environment variables from file: ")
  (mapc (lambda(line)
          (when (string-match "\\(.+\\)=\\(.*\\)" line)
            (setenv (match-string-no-properties 1 line)
                    (substitute-env-vars
                     (match-string-no-properties 2 line)))))
        (env-parse-utils-read-file-into-list-of-lines file)))

(defun env-parse-utils-load-env-var-from-file (var file &optional sep)
  "Load into the environment variable VAR each line from FILE.
Existing values will be maintained.  SEP is an optional separator."
  (interactive)
  (unless sep (setq sep path-separator))
  (setenv var (concat (mapconcat 'convert-standard-filename
                                 (env-parse-utils-read-file-into-list-of-lines file)
                                 sep) sep (getenv var))))

(provide 'env-parse-utils)
;;; env-parse-utils.el ends here
