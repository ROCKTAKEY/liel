;;; liel.el --- Lisp interpreter written in Emacs Lisp  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  ROCKTAKEY

;; Author: ROCKTAKEY <rocktakey@gmail.com>
;; Keywords: lisp

;; Version: 0.0.0
;; Package-Requires: ((emacs "24.1"))
;; URL: https://github.com/ROCKTAKEY/liel

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

;; Lisp interpreter written in Emacs Lisp

;;; Code:

(require 'cl-lib)

(defgroup liel ()
  "Lisp interpreter written in Emacs Lisp"
  :group 'lisp
  :prefix "liel-"
  :link '(url-link "https://github.com/ROCKTAKEY/liel"))

(defun liel-get-parenthesis-char-alist (plist)
  (plist-get plist :parenthesis-char-alist))

(defun liel-get-escape-char-list (plist)
  ""
  (plist-get plist :escape-char-list))

(defun liel-get-new-token-char-list (plist)
  ""
  (let ((parenthesis-char-alist (liel-get-parenthesis-char-alist plist))
        (string-quote-char-alist (liel-get-string-quote-char-alist plist)))
    (cl-remove-duplicates
     (mapcan
      (lambda (alist)
        (mapcan
         (lambda (arg) (list (car arg) (cdr arg)))
         alist))
      (list parenthesis-char-alist string-quote-char-alist)))))

(defun liel-get-token-separator-char-list (plist)
  ""
  (plist-get plist :token-separator-char-list))

(defun liel-get-end-token-char-list (plist)
  ""
  (let ((parenthesis-alist (liel-get-parenthesis-char-alist plist)))
    (cl-remove-duplicates
     (mapcan
      (lambda (arg) (list (car arg) (cdr arg)))
      parenthesis-alist))))

(defun liel-get-string-quote-char-alist (plist)
  ""
  (plist-get plist :string-quote-char-alist))

(defun liel-get-string-escape-char-list (plist)
  ""
  (plist-get plist :string-escape-char-list))

(defun liel-read-token-string (plist input position)
  ""
  (let (char-list
        close-quote
        escape?
        (count position)
        break

        (token-separator-char-list (liel-get-token-separator-char-list plist))
        (string-escape-char-list (liel-get-string-escape-char-list plist))
        (string-quote-char-alist (liel-get-string-quote-char-alist plist)))

    (while (memq (aref input count) token-separator-char-list)
      (cl-incf count))

    (setq close-quote (alist-get (aref input count) string-quote-char-alist))

    (cl-assert close-quote)

    (cl-incf count)

    (while (not break)
      (let ((char (aref input count)))
        (cond
         (escape?
          (push char char-list)
          (setq escape? nil))
         ((eq char close-quote)
          (setq break 'in-token))
         ((memq char string-escape-char-list)
          (setq escape? t)
          (push char char-list))
         (t
          (push char char-list)))

        (cl-incf count)))

    (list :token (apply #'string (reverse char-list))
          :input (if (eq break 'in-token)
                     count
                   (1- count))
          :string? t)))

(defun liel-read-token (plist input position)
  ""
  (let (char-list
        escape?
        (count position)
        break

        (escape-char-list (liel-get-escape-char-list plist))
        (new-token-char-list (liel-get-new-token-char-list plist))
        (end-token-char-list (liel-get-end-token-char-list plist))
        (token-separator-char-list (liel-get-token-separator-char-list plist))
        (string-quote-char-alist (liel-get-string-quote-char-alist plist)))

    (while (memq (aref input count) token-separator-char-list)
      (cl-incf count))

    (if (alist-get (aref input count) string-quote-char-alist)
        (liel-read-token-string plist input count)
      (while (not break)
        (let ((char (aref input count)))
          (cond
           (escape?
            (push char char-list)
            (setq escape? nil))

           ((memq char escape-char-list)
            (push char char-list)
            (setq escape? t))

           ((memq char token-separator-char-list)
            (setq break 'out-of-token))

           ((and (null char-list)
                 (memq char new-token-char-list)
                 (memq char end-token-char-list))
            (push char char-list)
            (setq break 'in-token))

           ((and char-list
                 (memq char new-token-char-list))
            (setq break 'out-of-token))

           ((memq char end-token-char-list)
            (push char char-list)
            (setq break 'in-token))

           (t
            (push char char-list)))
          (cl-incf count)))
      (list :token (apply #'string (reverse char-list))
            :input (if (eq break 'in-token)
                       count
                     (1- count))
            :string? nil))))

(provide 'liel)
;;; liel.el ends here
