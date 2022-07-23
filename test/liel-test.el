;;; liel-test.el --- Test for liel

;; Copyright (C) 2022  ROCKTAKEY

;; Author: ROCKTAKEY <rocktakey@gmail.com>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Test for liel

;;; Code:

(require 'ert)

(require 'undercover)
(undercover "*.el"
            (:report-format 'codecov)
            (:report-file "coverage-final.json")
            (:send-report nil))

(require 'liel)

(ert-deftest liel-read-token-symbol ()
  (let ((plist '( :escape-char-list (?\\)
                  :new-token-char-list (?\( ?\) ?\")
                  :end-token-char-list (?\( ?\))
                  :token-separator-char-list (?\ )
                  :string-escape-char-list (?\\)
                  :string-quote-char-alist ((?\" . ?\"))
                  )))

    (let ((token-plist (liel-read-token plist
                                        "abc def ghi" 0)))
      (should (string= (plist-get token-plist :token)
                       "abc")))

    (let ((token-plist (liel-read-token plist
                                        "(abc def ghi)" 0)))
      (should (string= (plist-get token-plist :token)
                       "(")))

    (let ((token-plist (liel-read-token plist
                                        "xyz(abc def ghi)" 0)))
      (should (string= (plist-get token-plist :token)
                       "xyz")))

    (let ((token-plist (liel-read-token plist
                                        "((abc def ghi))" 0)))
      (should (string= (plist-get token-plist :token)
                       "(")))

    (let ((token-plist (liel-read-token plist
                                        "\\(abc def ghi)" 0)))
      (should (string= (plist-get token-plist :token)
                       "\\(abc")))

    (let ((token-plist (liel-read-token plist
                                        "\\ abc def ghi)" 0)))
      (should (string= (plist-get token-plist :token)
                       "\\ abc")))

    (let ((token-plist (liel-read-token plist
                                        "ab\\(c def ghi)" 0)))
      (should (string= (plist-get token-plist :token)
                       "ab\\(c")))

    (let ((token-plist (liel-read-token plist
                                        "ab\\ c def ghi)" 0)))
      (should (string= (plist-get token-plist :token)
                       "ab\\ c")))

    (let ((token-plist (liel-read-token plist
                                        "def\"abc\" ghi)" 0)))
      (should (string= (plist-get token-plist :token)
                       "def")))))

(ert-deftest liel-read-token-string ()
  (let ((plist '( :escape-char-list (?\\)
                  :new-token-char-list (?\( ?\) ?\")
                  :end-token-char-list (?\( ?\))
                  :token-separator-char-list (?\ )
                  :string-escape-char-list (?\\)
                  :string-quote-char-alist ((?\" . ?\"))
                  )))
    (let ((token-plist (liel-read-token plist
                                        "\"abc\" def ghi)" 0)))
      (should (string= (plist-get token-plist :token)
                       "abc"))
      (should (plist-get token-plist :string?)))

    (let ((token-plist (liel-read-token plist
                                        "\"abc\"def ghi)" 0)))
      (should (string= (plist-get token-plist :token)
                       "abc"))
      (should (plist-get token-plist :string?)))

    (let ((token-plist (liel-read-token plist
                                        "\"abc\"\"def\" ghi)" 0)))
      (should (string= (plist-get token-plist :token)
                       "abc"))
      (should (plist-get token-plist :string?)))))

(provide 'liel-test)
;;; liel-test.el ends here
