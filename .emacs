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

(define-key global-map (kbd "RET") 'newline-and-indent)
(global-linum-mode t)
(tool-bar-mode -1)

; Disable backup
(setq backup-inhibited nil)
; Disable auto-save
(setq auto-save-default nil)

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
