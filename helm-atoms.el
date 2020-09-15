;;; helm-atoms.el -- Find atom using helm

;;; Commentary:

;;; Code:

(require 'helm)

(defun helm-atoms--string-atom (atom)
  "Return (name . value) for ATOMs that are strings."
  (when (and (boundp atom)
             (stringp (symbol-value atom))
             (not (string-match-p "\n" (symbol-value atom))))
    (cons (format "%s: %s" (propertize (symbol-name atom) 'face font-lock-variable-name-face) (symbol-value atom))
          atom)))

(defun helm-atoms--create-atom-list ()
  "Create helm caididates for helm-atoms."
  (let (candidates)
    (mapatoms (lambda (atom)
                (when-let (val (helm-atoms--string-atom atom))
                  (push val candidates))))
    candidates))

(defvar helm-source-atoms
      (helm-build-sync-source "String atoms"
        :candidates #'helm-atoms--create-atom-list
        :action #'identity)
        "Helm source for helm-atoms.")

(defun helm-atoms ()
  "Use helm to search through all single line strinng atoms."
  (interactive)
  (helm :sources '(helm-source-atoms)))


(provide 'helm-atoms)
;;; helm-atoms.el ends here
