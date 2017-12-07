#!/bin/sh
":"; exec "$EMACSX" --quick --script "$0" -- "$@" # -*- mode: emacs-lisp; -*-
;;; test_parsenv-utils.el --- test env parse utilties
;; Copyright (C) 2017  Dan Harms (dharms)
;; Author: Dan Harms <enniomore@icloud.com>
;; Created: Monday, December  4, 2017
;; Version: 1.0
;; Modified Time-stamp: <2017-12-07 05:40:44 dharms>
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
;; Test parsenv utilities.
;;

;;; Code:
(load-file "test/parsenv-test-common.el")
(require 'parsenv)

(ert-deftest ert-parsenv-test-util-strip-comments ()
  (should (string= (parsenv-utils-strip-comments "")
                   ""))
  (should (string= (parsenv-utils-strip-comments "Hello")
                   "Hello"))
  (should (string= (parsenv-utils-strip-comments "Hello ")
                   "Hello "))
  (should (string= (parsenv-utils-strip-comments "Hello#")
                   "Hello"))
  (should (string= (parsenv-utils-strip-comments "Hello#there")
                   "Hello"))
  (should (string= (parsenv-utils-strip-comments "Hello# there")
                   "Hello"))
  (should (string= (parsenv-utils-strip-comments "Hello #")
                   "Hello "))
  (should (string= (parsenv-utils-strip-comments "Hello #there")
                   "Hello "))
  (should (string= (parsenv-utils-strip-comments "Hello # there")
                   "Hello "))
  (should (string= (parsenv-utils-strip-comments "#Hello")
                   ""))
  (should (string= (parsenv-utils-strip-comments "# Hello")
                   ""))
  (should (string= (parsenv-utils-strip-comments " #Hello")
                   " "))
  (should (string= (parsenv-utils-strip-comments " # Hello")
                   " "))
  )

(ert-deftest ert-parsenv-test-util-continuation-p ()
  (should (not (parsenv-utils-continuation-p "")))
  (should (not (parsenv-utils-continuation-p " ")))
  (should (not (parsenv-utils-continuation-p "a")))
  (should (not (parsenv-utils-continuation-p " abc")))
  (should (not (parsenv-utils-continuation-p "abc ")))
  (should (not (parsenv-utils-continuation-p "abc \\ ")))
  (should (parsenv-utils-continuation-p "\\"))
  (should (parsenv-utils-continuation-p " \\"))
  (should (parsenv-utils-continuation-p " abc \\"))
  (should (parsenv-utils-continuation-p " abc\\"))
  )

(ert-deftest ert-parsenv-test-util-strip-continuation ()
  (should (string= (parsenv-utils-strip-continuation "")
                   ""))
  (should (string= (parsenv-utils-strip-continuation "abc")
                   "abc"))
  (should (string= (parsenv-utils-strip-continuation "abc ")
                   "abc "))
  (should (string= (parsenv-utils-strip-continuation "abc \\")
                   "abc "))
  (should (string= (parsenv-utils-strip-continuation "abc\\")
                   "abc"))
  (should (string= (parsenv-utils-strip-continuation "abc \\ ")
                   "abc \\ "))
  (should (string= (parsenv-utils-strip-continuation "abc \\ def \\")
                   "abc \\ def "))
  )

(ert-deftest ert-parsenv-test-util-strip-export ()
  (should (string= (parsenv-utils-strip-export "")
                   ""))
  (should (string= (parsenv-utils-strip-export "key")
                   "key"))
  (should (string= (parsenv-utils-strip-export " key")
                   " key"))
  (should (string= (parsenv-utils-strip-export " key ")
                   " key "))
  (should (string= (parsenv-utils-strip-export "key=value")
                   "key=value"))
  (should (string= (parsenv-utils-strip-export "key=value ")
                   "key=value "))
  (should (string= (parsenv-utils-strip-export " key=value")
                   " key=value"))
  (should (string= (parsenv-utils-strip-export " key=value ")
                   " key=value "))
  (should (string= (parsenv-utils-strip-export "export key")
                   "key"))
  (should (string= (parsenv-utils-strip-export "export key=value")
                   "key=value"))
  (should (string= (parsenv-utils-strip-export " export key=value")
                   "key=value"))
  (should (string= (parsenv-utils-strip-export "	export key=value")
                   "key=value"))
  (should (string= (parsenv-utils-strip-export "export    key=value")
                   "key=value"))
  (should (string= (parsenv-utils-strip-export "export	key=value")
                   "key=value"))
  (should (string= (parsenv-utils-strip-export " 	 export 	 key=value")
                   "key=value"))
  )

(ert-deftest ert-parsenv-test-util-consolidate-continuations ()
  (should (equal (parsenv-utils-consolidate-continuations
                  '())
                 '()))
  (should (equal (parsenv-utils-consolidate-continuations
                  '("hello"))
                 '("hello")))
  (should (equal (parsenv-utils-consolidate-continuations
                  '("hello" "there"))
                 '("hello" "there")))
  (should (equal (parsenv-utils-consolidate-continuations
                  '("hello\\"))
                 '("hello")))
  (should (equal (parsenv-utils-consolidate-continuations
                  '("hello" "there\\"))
                 '("hello" "there")))
  (should (equal (parsenv-utils-consolidate-continuations
                  '("hello\\" "there"))
                 '("hellothere")))
  (should (equal (parsenv-utils-consolidate-continuations
                  '("hello\\" "there" "you"))
                 '("hellothere" "you")))
  (should (equal (parsenv-utils-consolidate-continuations
                  '("hello" "there\\" "you"))
                 '("hello" "thereyou")))
  )

(ert-run-tests-batch-and-exit (car argv))

;;; test_parsenv-utils.el ends here
