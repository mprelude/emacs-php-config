;;; load_modules --- Load all of the required modules.

;; Keywords: configuration, modules

;;; Commentary:

;; - Defines locations to load themes & modules from.
;; - Loads modules from disk, and automatically downloads others from MELPA
;; & Marmalade.

;;; Code:

;; Define place to load themes & modules from.
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
(add-to-list 'load-path "~/.emacs.d/modules/")

;; List of non-repo packages to load.
(require 'cl)
(require 'package)
(require 'php-cs-fixer)
(require 'php-doc)

;; Setup configuration for package repository.
;; Defines the places to download packages from.
(package-initialize)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(setq package-archive-enable-alist '(("melpa" deft magit)))

;; Set a list of packages which must be installed.
(defvar mp/pkgs '(ac-slime
                  auto-complete
                  auto-dim-other-buffers
                  autopair
                  deft
                  diff-hl
                  direx
                  feature-mode
                  find-file-in-project
                  flycheck
                  gist
                  grizzl
                  highlight-indentation
                  htmlize
                  icicles
                  jinja2-mode
                  markdown-mode
                  marmalade
                  nodejs-repl
                  org
                  paredit
                  php-auto-yasnippets
                  php-mode
                  popwin
                  powerline
                  projectile
                  puppet-mode
                  restclient
                  robe
                  rvm
                  smartparens
                  smerge-mode
                  smex
                  web-mode
                  websocket
                  workgroups2
                  writegood-mode
                  ws-butler
                  yaml-mode
                  yasnippet
                  zenburn-theme)
  "Default packages.")

(defun mp/packages-installed-p ()
  "Make sure that all packages are installed."
  (loop for pkg in mp/pkgs
        when (not (package-installed-p pkg)) do (return nil)
        finally (return t)))

;; If all of the packages aren't installed, go and download from MELPA/Marmalade.
(unless (mp/packages-installed-p)
  (message "%s" "Refreshing package database...")
  (package-refresh-contents)
  (dolist (pkg mp/pkgs)
    (when (not (package-installed-p pkg))
      (package-install pkg))))

(provide 'load-modules)
;;; load_modules.el ends here
