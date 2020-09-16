;;; helm-atoms.el -- Reverse variable lookup using Helm  -*- lexical-binding: t -*-

;; Copyright (C) 2020 Dante Catalfamo

;; Author: Dante Catalfamo
;; Version: 0.1.2
;; Package-Requires: ((emacs "25.1") (helm))
;; URL: https://github.com/dantecatalfamo/helm-atoms

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Perform reverse variable lookups for Emacs Lisp using Helm.
;; Works on numbers and multiline strings.

;;; Commands

;; `helm-atoms' Open the helm interface

;;; Code:

(require 'helm)

(defun helm-atoms--string-atom (atom)
  "Return ((\"name: value\" . name) ...) for each line in ATOM."
  (when (boundp atom)
    (let ((value (symbol-value atom)))
      (when (numberp value)
        (setq value (number-to-string value)))
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

(defun helm-atoms ()
  "Reverse variable lookup using helm."
  (interactive)
  (let ((symbol (helm :sources '(helm-source-atoms))))
    (unless (null symbol)
      (if (require 'helpful nil 'noerror)
          (helpful-variable symbol)
        (describe-variable symbol)))))


(provide 'helm-atoms)
;;; helm-atoms.el ends here
