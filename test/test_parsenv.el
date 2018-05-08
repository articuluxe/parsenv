#!/bin/sh
":"; exec "$VISUAL" --quick --script "$0" -- "$@" # -*- mode: emacs-lisp; -*-
;;; test_parsenv.el --- test env parse utilties
;; Copyright (C) 2017-2018  Dan Harms (dharms)
;; Author: Dan Harms <enniomore@icloud.com>
;; Created: Monday, December  4, 2017
;; Version: 1.0
;; Modified Time-stamp: <2018-05-08 16:19:08 dan.harms>
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
;; Test parsenv utilities.
;;

;;; Code:
(load-file "test/parsenv-test-common.el")
(require 'parsenv)

(ert-deftest ert-parsenv-test-strip-comments ()
  (should (string= (parsenv-strip-comments "")
                   ""))
  (should (string= (parsenv-strip-comments "Hello")
                   "Hello"))
  (should (string= (parsenv-strip-comments "Hello ")
                   "Hello "))
  (should (string= (parsenv-strip-comments "Hello#")
                   "Hello"))
  (should (string= (parsenv-strip-comments "Hello#there")
                   "Hello"))
  (should (string= (parsenv-strip-comments "Hello# there")
                   "Hello"))
  (should (string= (parsenv-strip-comments "Hello #")
                   "Hello "))
  (should (string= (parsenv-strip-comments "Hello #there")
                   "Hello "))
  (should (string= (parsenv-strip-comments "Hello # there")
                   "Hello "))
  (should (string= (parsenv-strip-comments "#Hello")
                   ""))
  (should (string= (parsenv-strip-comments "# Hello")
                   ""))
  (should (string= (parsenv-strip-comments " #Hello")
                   " "))
  (should (string= (parsenv-strip-comments "\"Hello\"")
                   "\"Hello\""))
  (should (string= (parsenv-strip-comments "\"Hello\"")
                   "\"Hello\""))
  (should (string= (parsenv-strip-comments "\"Hello#\"")
                   "\"Hello#\""))
  (should (string= (parsenv-strip-comments "\"Hello#there\" #theend")
                   "\"Hello#there\" "))
  (should (string= (parsenv-strip-comments " \"Hello # there \"  # a comment ")
                   " \"Hello # there \"  "))
  (should (string= (parsenv-strip-comments " \"########\"########")
                   " \"########\""))
  (should (string= (parsenv-strip-comments " \"##\" \"##\"# a comment ")
                   " \"##\" \"##\""))
  )

(ert-deftest ert-parsenv-test-continuation-p ()
  (should (not (parsenv-continuation-p "")))
  (should (not (parsenv-continuation-p " ")))
  (should (not (parsenv-continuation-p "a")))
  (should (not (parsenv-continuation-p " abc")))
  (should (not (parsenv-continuation-p "abc ")))
  (should (not (parsenv-continuation-p "abc \\ ")))
  (should (parsenv-continuation-p "\\"))
  (should (parsenv-continuation-p " \\"))
  (should (parsenv-continuation-p " abc \\"))
  (should (parsenv-continuation-p " abc\\"))
  )

(ert-deftest ert-parsenv-test-strip-continuation ()
  (should (string= (parsenv-strip-continuation "")
                   ""))
  (should (string= (parsenv-strip-continuation "abc")
                   "abc"))
  (should (string= (parsenv-strip-continuation "abc ")
                   "abc "))
  (should (string= (parsenv-strip-continuation "abc \\")
                   "abc "))
  (should (string= (parsenv-strip-continuation "abc\\")
                   "abc"))
  (should (string= (parsenv-strip-continuation "abc \\ ")
                   "abc \\ "))
  (should (string= (parsenv-strip-continuation "abc \\ def \\")
                   "abc \\ def "))
  )

(ert-deftest ert-parsenv-test-strip-export ()
  (should (string= (parsenv-strip-export "")
                   ""))
  (should (string= (parsenv-strip-export "key")
                   "key"))
  (should (string= (parsenv-strip-export " key")
                   " key"))
  (should (string= (parsenv-strip-export " key ")
                   " key "))
  (should (string= (parsenv-strip-export "key=value")
                   "key=value"))
  (should (string= (parsenv-strip-export "key=value ")
                   "key=value "))
  (should (string= (parsenv-strip-export " key=value")
                   " key=value"))
  (should (string= (parsenv-strip-export " key=value ")
                   " key=value "))
  (should (string= (parsenv-strip-export "export key")
                   "key"))
  (should (string= (parsenv-strip-export "export key=value")
                   "key=value"))
  (should (string= (parsenv-strip-export " export key=value")
                   "key=value"))
  (should (string= (parsenv-strip-export "  export key=value")
                   "key=value"))
  (should (string= (parsenv-strip-export "export    key=value")
                   "key=value"))
  (should (string= (parsenv-strip-export "export    key=value")
                   "key=value"))
  (should (string= (parsenv-strip-export "   export      key=value")
                   "key=value"))
  )

(ert-deftest ert-parsenv-test-strip-setenv ()
  (should (string= (parsenv-strip-export "setenv key")
                   "key"))
  (should (string= (parsenv-strip-export "setenv key ")
                   "key "))
  (should (string= (parsenv-strip-export " setenv key ")
                   "key "))
  (should (string= (parsenv-strip-export "setenv key value")
                   "key value"))
  (should (string= (parsenv-strip-export " setenv key value")
                   "key value"))
  (should (string= (parsenv-strip-export "setenv key value ")
                   "key value "))
  (should (string= (parsenv-strip-export " setenv key value ")
                   "key value "))
  )

(ert-deftest ert-parsenv-test-strip-set ()
  (should (string= (parsenv-strip-export "set key")
                   "key"))
  (should (string= (parsenv-strip-export "set key ")
                   "key "))
  (should (string= (parsenv-strip-export " set key")
                   "key"))
  (should (string= (parsenv-strip-export "set key=value")
                   "key=value"))
  (should (string= (parsenv-strip-export " set key=value")
                   "key=value"))
  (should (string= (parsenv-strip-export "set key=value ")
                   "key=value "))
  (should (string= (parsenv-strip-export " set key=value ")
                   "key=value "))
  )

(ert-deftest ert-parsenv-test-consolidate-continuations ()
  (should (equal (parsenv-consolidate-continuations
                  '())
                 '()))
  (should (equal (parsenv-consolidate-continuations
                  '("hello"))
                 '("hello")))
  (should (equal (parsenv-consolidate-continuations
                  '("hello" "there"))
                 '("hello" "there")))
  (should (equal (parsenv-consolidate-continuations
                  '("hello\\"))
                 '("hello")))
  (should (equal (parsenv-consolidate-continuations
                  '("hello" "there\\"))
                 '("hello" "there")))
  (should (equal (parsenv-consolidate-continuations
                  '("hello\\" "there"))
                 '("hellothere")))
  (should (equal (parsenv-consolidate-continuations
                  '("hello\\" "there" "you"))
                 '("hellothere" "you")))
  (should (equal (parsenv-consolidate-continuations
                  '("hello" "there\\" "you"))
                 '("hello" "thereyou")))
  )

(ert-deftest ert-parsenv-test-extract-key-value ()
  (should (equal (parsenv-extract-key-value "")
                 '("" "")))
  (should (equal (parsenv-extract-key-value "key")
                 '("key" "")))
  (should (equal (parsenv-extract-key-value "key value")
                 '("key value" "")))
  (should (equal (parsenv-extract-key-value "key=value")
                 '("key" "value")))
  (should (equal (parsenv-extract-key-value "key=")
                 '("key" "")))
  )

(ert-deftest ert-parsenv-test-delimited-by-p ()
  (should (not (parsenv-delimited-by-p "" ?')))
  (should (not (parsenv-delimited-by-p "abc" ?')))
  (should (not (parsenv-delimited-by-p "'abc" ?')))
  (should (not (parsenv-delimited-by-p "abc'" ?')))
  (should (not (parsenv-delimited-by-p "\"abc\"" ?')))
  (should (not (string= (parsenv-delimited-by-p "'abc'" ?\")
                        "abc")))
  (should (string= (parsenv-delimited-by-p "'abc'" ?')
                   "abc"))
  )

(ert-deftest ert-parsenv-test-parse-environment ()
  (let ((process-environment nil))
    (parsenv-parse-lines '())
    (should (equal process-environment
                   '()
                   )))
  (let ((process-environment nil))
    (parsenv-parse-lines '("hello"))
    (should (equal process-environment
                   '("hello")
                   )))
  (let ((process-environment nil))
    (parsenv-parse-lines '("hello="))
    (should (equal process-environment
                   '("hello")
                   )))
  (let ((process-environment nil))
    (parsenv-parse-lines '("hello=there"))
    (should (equal process-environment
                   '("hello=there")
                   )))
  (let ((process-environment nil))
    (parsenv-parse-lines '("hello=there" "key=value" "alone"))
    (should (equal process-environment
                   '("alone"
                     "key=value"
                     "hello=there")
                   ))
    (should (string= (getenv "hello") "there"))
    (should (string= (getenv "key") "value"))
    (should (string= (getenv "alone") nil))
    )

  (let ((process-environment '("orig=initial")))
    (parsenv-parse-lines '("key=value"
                           "key2=\"$orig\""
                           "key3=$key"
                           ))
    (should (equal process-environment
                   '("key3=value"
                     "key2=initial"
                     "key=value"
                     "orig=initial"
                     )))
    (should (string= (getenv "orig") "initial"))
    (should (string= (getenv "key") "value"))
    (should (string= (getenv "key2") "initial"))
    (should (string= (getenv "key3") "value"))
    (should (string= (getenv "missing") nil))
    )
  )

(ert-deftest ert-parsenv-test-full ()
  (let ((process-environment nil)
        (lst '("#This is a comment"
               "export	key=value"
               "export key2=value2  #comment"
               " export key3=\\"
               "value3  # \\not a continuation"
               "export key4=fourth\\ "
               "export key5=a-#-comment-here"
               "export key6=\"no-#-comment-here\""
               )))
    (parsenv-parse-lines (parsenv-transform-lines lst))
    (should (string= (getenv "missing") nil))
    (should (string= (getenv "key") "value"))
    (should (string= (getenv "key2") "value2"))
    (should (string= (getenv "key3") "value3"))
    (should (string= (getenv "key4") "fourth\\"))
    (should (string= (getenv "key5") "a-"))
    (should (string= (getenv "key6") "no-#-comment-here"))
    )
  (let ((process-environment '("orig=initial"))
        (lst '("#Comment"
               "export  key1=$orig	"
               "export	key2=\"$orig\" "
               "export key3='$orig'"
               "export key4=\"my${orig}value\""
               " export key5=$key2   "
               )))
    (parsenv-parse-lines (parsenv-transform-lines lst))
    (should (string= (getenv "missing") nil))
    (should (string= (getenv "key1") "initial"))
    (should (string= (getenv "key2") "initial"))
    (should (string= (getenv "key3") "$orig"))
    (should (string= (getenv "key4") "myinitialvalue"))
    (should (string= (getenv "key5") "initial"))
    )
  (let ((process-environment '("orig=initial"))
        (lst '(" setenv  orig=new"
               "SET key1=value1 #comment"
               "set  key2=\"value2\""
               " SET \"key3=value3\""
               "SET \"key4=\""
               )))
    (parsenv-parse-lines (parsenv-transform-lines lst))
    (should (string= (getenv "missing") nil))
    (should (string= (getenv "orig") "new"))
    (should (string= (getenv "key1") "value1"))
    (should (string= (getenv "key2") "value2"))
    (should (string= (getenv "key3") "value3"))
    (should (string= (getenv "key4") nil))
    )
  )

(ert-deftest ert-parsenv-test-adjust-exec-path ()
  (let ((process-environment nil))
    (setenv "PATH" "one:two:three")
    (parsenv-adjust-exec-path)
    (should (equal exec-path
                     `("one/"
                       "two/"
                       "three/"
                       ,(file-name-as-directory exec-directory)
                       ))))
  )

(ert-run-tests-batch-and-exit (car argv))

;;; test_parsenv.el ends here
