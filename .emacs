(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (tango-dark)))
 '(inhibit-startup-screen t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

; Auto-indentation
(define-key global-map (kbd "RET") 'newline-and-indent)
; Show line number
(global-linum-mode t)
; Hide the tool-bar
(tool-bar-mode -1)

; Disable auto-save
(setq auto-save-default nil)
; Disable the creation of backup files
(setq make-backup-files nil)

; Disable the check for modified buffers when exiting
(defun my-kill-emacs ()
  (interactive)
  (save-some-buffers nil t)
  (kill-emacs))
(global-set-key (kbd "C-x C-c") 'my-kill-emacs)

(setq c-default-style
      '((java-mode . "java")
	(awk-mode . "awk")
	(other . "k&r")))

(setenv "PATH" (concat "/usr/lib/smlnj/bin:" (getenv "PATH")))
(setq exec-path (cons "/usr/lib/smlnj/bin" exec-path))

(autoload 'python-mode "python-mode.el" "Python mode." t)
(setq auto-mode-alist (append '(("/*.\.py$" . python-mode)) auto-mode-alist))

(setq auto-mode-alist (cons '("\.lua$" . lua-mode) auto-mode-alist))
(autoload 'lua-mode "lua-mode" "Lua editing mode." t)

(setq auto-mode-alist (cons '("\.ws$" . whitespace-mode) auto-mode-alist))
(autoload 'whitespace-mode "~/.emacs.d/whitespace-mode.el" "Whitespace editing mode." t)
