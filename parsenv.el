;;; parsenv.el --- parse .env files
;; Copyright (C) 2017  Dan Harms (dharms)
;; Author: Dan Harms <enniomore@icloud.com>
;; Created: Monday, December  4, 2017
;; Version: 1.0
;; Modified Time-stamp: <2017-12-05 17:48:53 dharms>
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
;; Parse .env files.
;;

;;; Code:
(require 'parsenv-utils)

(defun parsenv-utils-load-env-vars-from-file (file)
  "Load each line from FILE, of the form `var=val'.
For each line, sets environment variable `var' equal to `val'."
  (interactive "fLoad environment variables from file: ")
  (mapc (lambda(line)
          (when (string-match "\\(.+\\)=\\(.*\\)" line)
            (setenv (match-string-no-properties 1 line)
                    (substitute-env-vars
                     (match-string-no-properties 2 line)))))
        (parsenv-utils-read-file-into-list-of-lines file)))

(defun parsenv-utils-load-env-var-from-file (var file &optional sep)
  "Load into the environment variable VAR each line from FILE.
Existing values will be maintained.  SEP is an optional separator."
  (interactive)
  (unless sep (setq sep path-separator))
  (setenv var (concat (mapconcat 'convert-standard-filename
                                 (parsenv-utils-read-file-into-list-of-lines file)
                                 sep) sep (getenv var))))

(provide 'parsenv)
;;; parsenv.el ends here
