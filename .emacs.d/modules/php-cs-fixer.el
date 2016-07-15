;;; php-cs-fixer.el ---

;; Copyright 2015 OVYA (Renée Costes Viager Group). All rights reserved.
;; Use of this source code is governed by a BSD-style
;; license that can be found in the LICENSE file.

;; Author: Philippe Ivaldi for OVYA
;; Source: Some pieces of code are copied from go-mode.el https://github.com/dominikh/go-mode.el
;; Version: 1.0.0
;; Keywords: languages php
;; URL: …
;;
;; This file is not part of GNU Emacs.

;;; Code:

(defcustom php-cs-fixer-command "php-cs-fixer"
  "The 'php-cs-fixer' command. See http://cs.sensiolabs.org/ for options"
  :type 'string
  :group 'php-cs-fixer)

(defcustom php-cs-fixer-level-option "symfony"
  "The 'php-cs-fixer' --level option value. See http://cs.sensiolabs.org/ for options"
  :type '(choice (const :tag "Not set" :value nil)
		 (const :value "psr0")
		 (const :value "psr1")
		 (const :value "psr2")
		 (const :value "symfony"))
  :group 'php-cs-fixer)

(defcustom php-cs-fixer-fixers-options
  '("multiline_spaces_before_semicolon" "concat_with_spaces")
  "The 'php-cs-fixer' --fixers option value. See http://cs.sensiolabs.org/"
  :type '(repeat string)
  :group 'php-cs-fixer)

;; Copy of go--goto-line from https://github.com/dominikh/go-mode.el
(defun php-cs-fixer--goto-line (line)
  (goto-char (point-min))
  (forward-line (1- line)))

;; Copy of go--delete-whole-line from https://github.com/dominikh/go-mode.el
(defun php-cs-fixer--delete-whole-line (&optional arg)
  "Delete the current line without putting it in the kill-ring."
  ;; Derived from `kill-whole-line'.
  ;; ARG is defined as for that function.
  (setq arg (or arg 1))
  (if (and (> arg 0)
	   (eobp)
	   (save-excursion (forward-visible-line 0) (eobp)))
      (signal 'end-of-buffer nil))
  (if (and (< arg 0)
	   (bobp)
	   (save-excursion (end-of-visible-line) (bobp)))
      (signal 'beginning-of-buffer nil))
  (cond ((zerop arg)
	 (delete-region (progn (forward-visible-line 0) (point))
			(progn (end-of-visible-line) (point))))
	((< arg 0)
	 (delete-region (progn (end-of-visible-line) (point))
			(progn (forward-visible-line (1+ arg))
			       (unless (bobp)
				 (backward-char))
			       (point))))
	(t
	 (delete-region (progn (forward-visible-line 0) (point))
			(progn (forward-visible-line arg) (point))))))

;; Derivated of go--apply-rcs-patch from https://github.com/dominikh/go-mode.el
(defun php-cs-fixer--apply-rcs-patch (patch-buffer)
    "Apply an RCS-formatted diff from PATCH-BUFFER to the current
buffer."
    (let ((target-buffer (current-buffer))
	  ;; Relative offset between buffer line numbers and line numbers
	  ;; in patch.
	  ;;
	  ;; Line numbers in the patch are based on the source file, so
	  ;; we have to keep an offset when making changes to the
	  ;; buffer.
	  ;;
	  ;; Appending lines decrements the offset (possibly making it
	  ;; negative), deleting lines increments it. This order
	  ;; simplifies the forward-line invocations.
	  (line-offset 0))
      (save-excursion
	(with-current-buffer patch-buffer
	  (goto-char (point-min))
	  (while (not (eobp))
	    (unless (looking-at "^\\([ad]\\)\\([0-9]+\\) \\([0-9]+\\)")
	      (error "invalid rcs patch or internal error in php-cs-fixer--apply-rcs-patch"))
	    (forward-line)
	    (let ((action (match-string 1))
		  (from (string-to-number (match-string 2)))
		  (len  (string-to-number (match-string 3))))
	      (cond
	       ((equal action "a")
		(let ((start (point)))
		  (forward-line len)
		  (let ((text (buffer-substring start (point))))
		    (with-current-buffer target-buffer
		      (decf line-offset len)
		      (goto-char (point-min))
		      (forward-line (- from len line-offset))
		      (insert text)))))
	       ((equal action "d")
		(with-current-buffer target-buffer
		  (php-cs-fixer--goto-line (- from line-offset))
		  (incf line-offset len)
		  (php-cs-fixer--delete-whole-line len)))
	       (t
		(error "invalid rcs patch or internal error in php-cs-fixer--apply-rcs-patch")))))))))

(defun php-cs-fixer--kill-error-buffer (errbuf)
  (let ((win (get-buffer-window errbuf)))
    (if win
	(quit-window t win)
      (kill-buffer errbuf))))

;;;###autoload
(defun php-cs-fix ()
  "Formats the current PHP buffer according to the PHP-CS-Fixer tool."
  (interactive)
  (let ((tmpfile (make-temp-file "PHP-CS-Fixer" nil ".php"))
	(patchbuf (get-buffer-create "*PHP-CS-Fixer patch*"))
	(errbuf (get-buffer-create "*PHP-CS-Fixer Errors*"))
	(coding-system-for-read 'utf-8)
	(coding-system-for-write 'utf-8))

    (save-restriction
      (widen)
      (if errbuf
	  (with-current-buffer errbuf
	    (setq buffer-read-only nil)
	    (erase-buffer)))
      (with-current-buffer patchbuf
	(erase-buffer))

      (write-region nil nil tmpfile)

      ;; We're using errbuf for the mixed stdout and stderr output. This
      ;; is not an issue because  php-cs-fixer -q does not produce any stdout
      ;; output in case of success.
      (if (zerop (call-process "php" nil errbuf nil "-l" tmpfile))
	  (progn
	    (call-process php-cs-fixer-command
			  nil errbuf nil
			  "fix"
			  (if php-cs-fixer-level-option (concat "--level=" php-cs-fixer-level-option) "")
			  (concat "--fixers=-psr0" ;; Because tmpfile can not support this constraint
				  (if php-cs-fixer-fixers-options (concat "," (mapconcat 'identity php-cs-fixer-fixers-options ",")) ""))
			  "--quiet"
			  tmpfile)
	    (if (zerop (call-process-region (point-min) (point-max) "diff" nil patchbuf nil "-n" "-" tmpfile))
		(message "Buffer is already php-cs-fixed")
	      (php-cs-fixer--apply-rcs-patch patchbuf)
	      (message "Applied php-cs-fixer")))
	(warn (with-current-buffer errbuf (buffer-string)))))

    (php-cs-fixer--kill-error-buffer errbuf)
    (kill-buffer patchbuf)
    (delete-file tmpfile)))

(defvar php-cs-fixer-command-not-found-msg "Package php-cs-fixer loaded but command line php-cs-fixer not found.
Fix this issue removing the Emacs package php-cs-fixer or installing the program php-cs-fixer")

;;;###autoload
(defun php-cs-fixer-before-save ()
    "Add this to .emacs to run php-cs-fix on the current buffer when saving:
 (add-hook 'before-save-hook 'php-cs-fixer-before-save)."

    (interactive)
    (if (executable-find "php-cs-fixer")
	(when (and
	       buffer-file-name
	       (string= (file-name-extension buffer-file-name) "php")
	       (or (not (boundp 'geben-temporary-file-directory)) (not (string-match geben-temporary-file-directory (file-name-directory buffer-file-name))))
	       ) (php-cs-fix))
      (warn php-cs-fixer-command-not-found-msg)))

(if (not (executable-find "php-cs-fixer"))
    (warn php-cs-fixer-command-not-found-msg))

(provide 'php-cs-fixer)

;;; php-cs-fixer ends here
