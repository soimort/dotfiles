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

; Hide the tool-bar
(tool-bar-mode -1)
; Hide the menu-bar
(menu-bar-mode -1)
; Hide the tooltip
(tooltip-mode -1)
; Show line number
(global-linum-mode t)

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

; Auto-indentation
(define-key global-map (kbd "RET") 'newline-and-indent)
; No tabs
(setq-default indent-tabs-mode nil)

; Use K&R style
(setq c-default-style
      '((java-mode . "java")
	(awk-mode . "awk")
	(other . "k&r")))

; Enable ElScreen
(load "elscreen" "ElScreen" t)
(elscreen-start)
(global-set-key (kbd "<C-tab>") 'elscreen-next)
(global-set-key (kbd "<C-S-iso-lefttab>") 'elscreen-previous)

; Keyboard scroll one line at a time
(setq scroll-step 1)
; Scrolling down the view
(global-set-key (kbd "M-n") (lambda () (interactive) (scroll-up 1)))
(global-set-key (kbd "M-<down>") (lambda () (interactive) (scroll-up 4)))
; Scrolling up the view
(global-set-key (kbd "M-p") (lambda () (interactive) (scroll-down 1)))
(global-set-key (kbd "M-<up>") (lambda () (interactive) (scroll-down 4)))

(setenv "PATH" (concat "/usr/lib/smlnj/bin:" (getenv "PATH")))
(setq exec-path (cons "/usr/lib/smlnj/bin" exec-path))

(autoload 'python-mode "python-mode.el" "Python mode." t)
(setq auto-mode-alist (append '(("/*.\.py$" . python-mode)) auto-mode-alist))

(setq auto-mode-alist (cons '("\.lua$" . lua-mode) auto-mode-alist))
(autoload 'lua-mode "lua-mode" "Lua editing mode." t)

(setq auto-mode-alist (cons '("\.ws$" . whitespace-mode) auto-mode-alist))
(autoload 'whitespace-mode "~/.emacs.d/whitespace-mode.el" "Whitespace editing mode." t)
