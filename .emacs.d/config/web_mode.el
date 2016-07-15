;;; web_mode --- Configuration for web-mode templates.

;; Keywords: configuration, web mode

;;; Commentary:

;; - Tells web-mode to render templates in another face.
;; - Tells web-mode it will be working with php.

;;; Code:

;; Render templating language differently to HTML.
(setq web-mode-enable-part-face t)
(setq web-mode-enable-block-face t)

;; Activate PHP mode in web-mode.
(add-hook 'web-mode-before-auto-complete-hooks
         '(lambda ()
         (let ((web-mode-cur-language
                 (web-mode-language-at-pos)))
         (if (string= web-mode-cur-language "php")
                 (yas-activate-extra-mode 'php-mode)
                 (yas-deactivate-extra-mode 'php-mode)))))

(provide 'web_mode)
;;; web_mode.el ends here
