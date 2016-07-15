;;; cleanup --- Defines defaults for clean editing & clean code.

;; Keywords: configuration, cleanup

;;; Commentary:

;; - Trims excess whitespace on saving.
;; - Sets a requirement for newlines on files.

;;; Code:

;; Enable autocomplete.
(ac-config-default)
(ac-config-default)
(setq ac-ignore-case nil)
(add-to-list 'ac-modes 'web-mode)
(add-hook 'after-init-hook #'global-flycheck-mode)

;; No tabs in files & tab width 4.
(setq tab-width 4
  indent-tabs-mode nil)

;; Whitespace cleaning.
(add-hook 'after-init-hook #'ws-butler-global-mode)

;; Indentation & buffer cleanup.
(defun untabify-buffer ()
  (interactive)
  (untabify (point-min) (point-max)))
(defun indent-buffer ()
  (interactive)
  (indent-region (point-min) (point-max)))
(defun cleanup-buffer ()
  "Perform a bunch of operations on the whitespace content of a buffer."
  (interactive)
  (indent-buffer)
  (untabify-buffer))
(defun cleanup-region (beg end)
  "Remove tmux artifacts from region."
  (interactive "r")
  (dolist (re '("\\\\│\·*\n" "\W*│\·*"))
  (replace-regexp re "" nil beg end)))
(setq-default show-trailing-whitespace t)

;; Merge mode prefix.
(setq smerge-command-prefix "\C-cv")

;; Require a newline at end of file.
(setq require-final-newline t)

(provide 'cleanup)
;;; cleanup.el ends here
