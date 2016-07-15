;;; php_mode_config --- Configuration for PHP mode & PHP-related plugins

;; Keywords: configuration, cleanup

;;; Commentary:

;; - PHP mode configuration.
;; - PHP codesniffer configuration.
;; - PHP doc configuration.

;;; Code:

;; PHP Documentation - World First standards.
(add-hook 'php-mode-hook
          (lambda ()
            (local-set-key (kbd "C-j") 'php-insert-doc-block)))
(setq php-insert-doc-varname-on-var t)
(setq php-insert-doc-access-tag nil)
(setq php-insert-doc-attribute-tags nil)
(setq php-insert-doc-uses-tag nil)
(setq php-insert-doc-author-name (getenv "USER_FULL_NAME"))
(setq php-insert-doc-author-email (getenv "USER_EMAIL_ADDRESS"))
(setq php-insert-doc-copyright-name (getenv "USER_COPYRIGHT_LINE"))
(setq php-insert-doc-copyright-email nil)

;; Need to set require final newline in php-mode specifically.
(add-hook 'php-mode-hook (lambda ()
                           (set (make-local-variable 'require-final-newline) t)))

;; Need to set up PHP argument lists.
(add-hook 'php-mode-hook (lambda ()
                           (defun ywb-php-lineup-arglist-intro (langelem)
                             (save-excursion
                               (goto-char (cdr langelem))
                               (vector (+ (current-column) c-basic-offset))))
                           (defun ywb-php-lineup-arglist-close (langelem)
                             (save-excursion
                               (goto-char (cdr langelem))
                               (vector (current-column))))
                           (c-set-offset 'arglist-intro 'ywb-php-lineup-arglist-intro)
                           (c-set-offset 'arglist-close 'ywb-php-lineup-arglist-close)))

(defun unindent-closure ()
  "Fix php-mode indent for closures."
  (let ((syntax (mapcar 'car c-syntactic-context)))
    (if (and (member 'arglist-cont-nonempty syntax)
             (or
              (member 'statement-block-intro syntax)
              (member 'brace-list-intro syntax)
              (member 'brace-list-close syntax)
              (member 'block-close syntax)))
        (save-excursion
          (beginning-of-line)
          (delete-char (* (count 'arglist-cont-nonempty syntax)
                          c-basic-offset))) )))

;; Fix broken closure indentation.
(add-hook 'php-mode-hook
          (lambda ()
            (add-hook 'c-special-indent-hook 'unindent-closure)))

;; Flycheck configuration.
(eval-after-load 'flycheck
  '(progn
     (set-face-attribute 'flycheck-error nil :background "darkred" :underline nil)
     (set-face-attribute 'flycheck-warning nil :background "red" :underline nil)
     (set-face-attribute 'flycheck-info nil :background "green" :underline nil)
     (flycheck-add-mode 'html-tidy 'web-mode)
     (flycheck-add-mode 'css-csslint 'web-mode)
     (flycheck-add-mode 'php 'php-mode)
     (flycheck-add-mode 'php-phpmd 'php-mode)
     (flycheck-add-mode 'php-phpcs 'php-mode)
     (setq flycheck-phpcs-standard "Symfony2")
     (setq flycheck-highlighting-mode 'lines)))

;; PHP CodeSniffer Fixer.
(setq php-cs-fixer-level-option "symfony")
(setq php-cs-fixer-fixers-options
      '("concat_with_spaces" "-psr0"))
(global-set-key (kbd "C-c M-p") 'php-cs-fix)

(provide 'php_mode_config)
;;; php_mode_config.el ends here
