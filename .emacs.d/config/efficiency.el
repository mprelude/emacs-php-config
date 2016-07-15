;;; efficiency --- Keep work as efficient as possible.

;; Keywords: configuration, efficiency

;;; Commentary:

;; - Key bindings.
;; - Theme.
;; - Shortcuts.

;;; Code:

;; Require long-form 'yes' to kill emacs.
(setq confirm-kill-emacs 'yes-or-no-p)

;; Indentation with spaces.
(setq-default indent-tabs-mode nil)

;; Smartparens setup.
(smartparens-global-mode)
(show-smartparens-global-mode t)
(sp-with-modes '(rhtml-mode)
         (sp-local-pair "<" ">")
         (sp-local-pair "<%" "%>"))

;; Setup Projectile.
(projectile-global-mode)
(setq projectile-enable-caching t)
(setq projectile-completion-system 'grizzl)
(global-set-key (kbd "M-p") 'projectile-find-file)
(global-set-key (kbd "M-b") 'projectile-switch-to-buffer)

;; Window configuration.
(setq column-number-mode t)
(tool-bar-mode -1)
(menu-bar-mode -1)
(setq scroll-preserve-screen-position t)
(setq scroll-error-top-bottom t)
(global-linum-mode)
(setq linum-format "%4d \u2502 ")

;; Deal with git changes, show in the right margin.
(setq diff-hl-side 'right)
(global-diff-hl-mode)
(diff-hl-margin-mode)
(diff-hl-flydiff-mode)
(add-hook 'window-configuration-change-hook
          (lambda ()
            (set-window-margins (car (get-buffer-window-list (current-buffer) nil t)) nil 2)))

;; Directory exploration.
(require 'popwin)
(popwin-mode 1)
(push '(direx:direx-mode :position left :width 25 :dedicated t)
      popwin:special-display-config)

;; Theme details.
(load-theme 'zenburn t)
(powerline-default-theme)
(icy-mode 1)
(global-hl-line-mode 1)
(set-face-background 'hl-line "#3e4446")
(set-face-foreground 'highlight nil)
(workgroups-mode 1)

;; Dim other buffers.
(add-hook 'after-init-hook (lambda ()
                             (when (fboundp 'auto-dim-other-buffers-mode)
                                   (auto-dim-other-buffers-mode t))))

;; Key bindings.
(windmove-default-keybindings)
(global-set-key (kbd "RET") 'newline-and-indent)
(global-set-key (kbd "C-;") 'comment-or-uncomment-region)
(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "C-c C-k") 'compile)
(global-set-key (kbd "C-x M-t") 'cleanup-region)
(global-set-key (kbd "C-c n") 'cleanup-buffer)
(global-set-key (kbd "C-c f") 'find-file-in-project)
(global-set-key (kbd "C-x C-j") 'direx:jump-to-directory-other-window)

(provide 'efficiency)
;;; efficiency.el ends here
