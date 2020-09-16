;;; helm-atoms.el -- Reverse variable lookup using Helm

;;; Commentary:

;;; Code:

(require 'helm)

(defun helm-atoms--string-atom (atom)
  "Return (name . value) for single line ATOMs."
  (when (boundp atom)
    (let ((value (symbol-value atom)))
      (when (numberp value)
        (setq value (number-to-string value)))
      (when (and (stringp value)
                 (not (string-match-p "\n" value)))
        (cons (format "%s: %s" (propertize (symbol-name atom) 'face font-lock-variable-name-face) value)
              atom)))))

(defun helm-atoms--create-atom-list ()
  "Create helm caididates for helm-atoms."
  (let (candidates)
    (mapatoms (lambda (atom)
                (when-let (val (helm-atoms--string-atom atom))
                  (push val candidates))))
    candidates))

(defvar helm-source-atoms
      (helm-build-sync-source "String / Number atoms"
        :candidates #'helm-atoms--create-atom-list
        :action #'identity)
        "Helm source for helm-atoms.")

(defun helm-atoms ()
  "Use helm to search through all single line strinng atoms."
  (interactive)
  (let ((symbol (helm :sources '(helm-source-atoms))))
    (unless (null symbol)
      (if (require 'helpful nil 'noerror)
          (helpful-variable symbol)
        (describe-variable symbol)))))



(provide 'helm-atoms)
;;; helm-atoms.el ends here
