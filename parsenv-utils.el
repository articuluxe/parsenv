;;; parsenv-utils.el --- utils for parsenv
;; Copyright (C) 2017  Dan Harms (dharms)
;; Author: Dan Harms <enniomore@icloud.com>
;; Created: Monday, December  4, 2017
;; Version: 1.0
;; Modified Time-stamp: <2017-12-05 17:48:54 dharms>
;; Modified by: Dan Harms
;; Keywords: tools
;; URL: https://github.com/articuluxe/parsenv.git
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

(defun parsenv-utils-trim (line)
  "Trim whitespace from beginning and end of LINE."
  (string-trim line))

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





(provide 'parsenv-utils)
;;; parsenv-utils.el ends here