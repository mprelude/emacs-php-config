;;; personal --- Define personal information.

;; Keywords: configuration, personal

;;; Commentary:

;; - Defines name and e-mail address.

;;; Code:

(setq user-full-name (getenv "USER_FULL_NAME"))
(setq user-mail-address (getenv "USER_EMAIL_ADDRESS"))

(provide 'personal)
;;; personal.el ends here
