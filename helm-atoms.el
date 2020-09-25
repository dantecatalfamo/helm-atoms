;;; helm-atoms.el --- Reverse variable lookup using Helm  -*- lexical-binding: t -*-

;; Copyright (C) 2020 Dante Catalfamo

;; Author: Dante Catalfamo
;; Version: 0.2.0
;; Package-Requires: ((emacs "25.1") (helm "2.0"))
;; URL: https://github.com/dantecatalfamo/helm-atoms
;; Keywords: help lisp maint helm tools matching

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
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

;; Perform an interactive reverse variable lookups for Emacs Lisp using Helm.
;; Works on numbers and strings, including multiline strings.
;; Once the helm entry is selected, the variable is opened in
;; either `describe-variable', or `helpful-variable', if it's installed.

;;; Commands

;; `helm-atoms' Open the helm interface

;;; Customization

;; `helm-atoms-search-sequences'
;; Search atoms bound to sequences (lists, vectors).
;; More thorough search, but worse performance.


;;; Code:

(require 'helm)

(declare-function helpful-variable "ext:helpful.el" t t) ; Prevent byte-compiler warnings

(defgroup helm-atoms nil
  "Reverse variable lookup using Helm."
  :group 'helm)

(defcustom helm-atoms-search-sequences nil
  "Search atoms bound to sequences (lists, vectors).
More thorough search, but worse performance."
  :type 'boolean
  :group 'helm-atoms)

(defun helm-atoms--string-atom (atom)
  "Return ((\"name: value\" . name) ...) for each line in ATOM."
  (when (boundp atom)
    (let ((value (symbol-value atom)))
      (when (numberp value)
        (setq value (number-to-string value)))
      (when (and (sequencep value)
                 helm-atoms-search-sequences)
        (setq value (prin1-to-string value)))
      (when (stringp value)
        (let (candidates)
          (dolist (line (split-string value "\n" t) candidates)
            (push (cons (format "%s: %s" (propertize (symbol-name atom) 'face font-lock-variable-name-face) line)
                        atom)
                  candidates)))))))

(defun helm-atoms--create-atom-list ()
  "Create helm caididates for `helm-atoms'."
  (let (candidates)
    (mapatoms (lambda (atom)
                (when-let (val (helm-atoms--string-atom atom))
                  (setq candidates (append val candidates)))))
    (nreverse candidates)))

(defvar helm-source-atoms
      (helm-build-sync-source "String / Number atoms"
        :candidates #'helm-atoms--create-atom-list
        :action #'identity)
        "Helm source for `helm-atoms'.")

;;;###autoload
(defun helm-atoms ()
  "Reverse variable lookup using helm."
  (interactive)
  (let ((symbol (helm :sources '(helm-source-atoms))))
    (when symbol
      (if (require 'helpful nil 'noerror)
          (helpful-variable symbol)
        (describe-variable symbol)))))


(provide 'helm-atoms)
;;; helm-atoms.el ends here
