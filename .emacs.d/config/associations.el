;;; associations --- Define associations between modes & file extensions.

;; Keywords: configuration, associations

;;; Commentary:

;; - Defines modes to run on each file extension.
;; - Define web mode engines for PHP.

;;; Code:

;; File associations to modes.
(add-to-list 'auto-mode-alist '("\\.php$" . php-mode))
(add-to-list 'auto-mode-alist '("\\.inc$" . php-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.phtml$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.zsh$" . shell-script-mode))
(add-to-list 'auto-mode-alist '("\\.gitconfig$" . conf-mode))
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.yaml$" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.mdown$" . markdown-mode))

;; Add web mode engines when dealing with php files.
(setq web-mode-engines-alist
      '(("php" . "\\.phtml")
        ("php" . "\\.tpl\\.php")))

(provide 'associations)
;;; associations.el ends here
