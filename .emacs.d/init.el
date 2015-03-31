(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.

 '(custom-enabled-themes (quote (tango-dark)))
 '(inhibit-startup-screen t) ; show *scratch* buffer at startup
 ;; Later: restore elscreen sessions or enter dired-mode

 ;; agda-mode
 '(agda2-highlight-level (quote non-interactive))
 '(agda2-include-dirs
   (quote ("." "/home/soimort/Source/agda-stdlib/src"))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.

 ;; elscreen
 '(elscreen-tab-background-face
   ((((type x w32 mac) (class color)) :background "#000f00")
    (((class color)) (:background "#000f00"))))
 '(elscreen-tab-current-screen-face
   ((((class color)) (:background "#aaeeaa" :foreground "#000000"))))
 '(elscreen-tab-other-screen-face
   ((((type x w32 mac) (class color)) :background "#224422" :foreground "#ffffff")))

 ;; powerline
 '(mode-line
   ((t (:background "#aaeeaa" :foreground "#000000" :box nil))))
 '(mode-line-inactive
   ((t (:background "#666666" :foreground "#f9f9f9" :box nil))))
 '(powerline-active1
   ((t (:background "#224422" :foreground "#ffffff" :inherit mode-line))))
 '(powerline-active2
   ((t (:background "#446644" :foreground "#ffffff" :inherit mode-line)))))



;; User Interface
;; --------------

;; Minimize UI
(tool-bar-mode -1) ; hide tool-bar
(menu-bar-mode -1) ; hide menu-bar
(tooltip-mode -1) ; hide tooltip

;; Set frame title
(setq frame-title-format "%b")

;; Set window transparency
(set-frame-parameter (selected-frame) 'alpha '(95 95))
(add-to-list 'default-frame-alist '(alpha 95 95))

;; Set default colors
(set-background-color "#000f00")
(set-face-background 'region "#103f10")
(set-cursor-color "#107f10")

;; Set default font family / size
(set-default-font "Inconsolata 13")

;; Show line numbers
(global-linum-mode t)

;; Show paren mode
(show-paren-mode 1)
;; Highlight parentheses
(set-face-background 'show-paren-match-face (face-background 'default))
(set-face-foreground 'show-paren-match-face "#ffff00")
(set-face-attribute 'show-paren-match-face nil :weight 'extra-bold)

;; Highlight current line
(global-hl-line-mode 1)
(set-face-background 'hl-line "#101f10")
(set-face-foreground 'highlight nil)

;; [M-n] Scrolling down the view (by 1 line)
(global-set-key (kbd "M-n") (lambda () (interactive) (scroll-up 1)))
(eval-after-load 'markdown-mode
  '(define-key markdown-mode-map
     (kbd "M-n") (lambda () (interactive) (scroll-up 1))))

;; [M-<down>] Scrolling down the view
(global-set-key (kbd "M-<down>") (lambda () (interactive) (scroll-up 4)))
(eval-after-load 'markdown-mode
  '(define-key markdown-mode-map
     (kbd "M-<down>") (lambda () (interactive) (scroll-up 4))))

;; [M-p] Scrolling up the view (by 1 line)
(global-set-key (kbd "M-p") (lambda () (interactive) (scroll-down 1)))
(eval-after-load 'markdown-mode
  '(define-key markdown-mode-map
      (kbd "M-p") (lambda () (interactive) (scroll-down 1))))

;; [M-<up>] Scrolling up the view
(global-set-key (kbd "M-<up>") (lambda () (interactive) (scroll-down 4)))
(eval-after-load 'markdown-mode
  '(define-key markdown-mode-map
     (kbd "M-<up>") (lambda () (interactive) (scroll-down 4))))

;; Change scaling factor
(setq text-scale-mode-step 1.05)

;; Enable mouse scroll-wheel scaling
(global-set-key (kbd "<C-mouse-4>") 'text-scale-increase)
(global-set-key (kbd "<C-mouse-5>") 'text-scale-decrease)

;; [C-` w] Close current window
(global-set-key (kbd "C-` w") 'delete-window)
;; [C-` h] Split window horizontally
(global-set-key (kbd "C-` h") 'split-window-right)
;; [C-` v] Split window vertically
(global-set-key (kbd "C-` v") 'split-window-below)

;; Purge minor modes in mode line
(defvar hidden-minor-modes
  '(abbrev-mode
    ibus-mode))
(defun purge-minor-modes ()
  (interactive)
  (dolist (x hidden-minor-modes nil)
    (let ((trg (cdr (assoc x minor-mode-alist))))
      (when trg
        (setcar trg "")))))
(add-hook 'after-change-major-mode-hook 'purge-minor-modes)



;; Editing
;; -------

;; Enable Common User Access (CUA) mode
(cua-mode 1)

;; Disable auto-save
(setq auto-save-default nil)
;; Disable the creation of backup files
(setq make-backup-files nil)

;; Disable the check for modified buffers when exiting
(defun my-kill-emacs ()
  (interactive)
  (save-some-buffers nil t)
  (kill-emacs))
(global-set-key (kbd "C-x C-c") 'my-kill-emacs)

;; Delete whole line without putting it into kill-ring
(defun delete-line (&optional arg)
  (interactive)
  (move-beginning-of-line 1)
  (kill-whole-line arg)
  (setq kill-ring (cdr kill-ring)))
(global-set-key (kbd "<C-S-backspace>") 'delete-line)

;; Auto-indentation
(define-key global-map (kbd "RET") 'newline-and-indent)
;; No tabs
(setq-default indent-tabs-mode nil)

;; Remove trailing whitespace when saving
(add-hook 'before-save-hook 'delete-trailing-whitespace)



;; Tools / Packages
;; ----------------

;; Use /bin/zsh as explicit shell
(setq explicit-shell-file-name "/bin/zsh")

;; [C-` s] Flyspell mode for comments and strings only
(global-set-key (kbd "C-` s") 'flyspell-prog-mode)
;; [C-` d] Flyspell mode
(global-set-key (kbd "C-` d") 'flyspell-mode)

;; Initialize MELPA
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
;; this could be freakin slow...
;(unless package-archive-contents (package-refresh-contents))
(package-initialize)



;; Coding Styles
;; -------------

;; C-like: K&R style
(setq c-default-style
      '((java-mode . "java")
	(awk-mode . "awk")
	(other . "k&r"))
      c-basic-offset 4)
(c-set-offset 'substatement-open 0)
(c-set-offset 'inline-open 0)

;; CSS
(setq css-indent-offset 2)

;; CoffeeScript
(setq coffee-tab-width 2)

;; Haskell
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)



;; Mode Tweaks
;; -----------

;; asm-mode: extra file associations
(add-to-list 'auto-mode-alist '("\.il$" . asm-mode))

;; emmet-mode: enable for editing XML and CSS
(add-hook 'sgml-mode-hook 'emmet-mode)
(add-hook 'css-mode-hook  'emmet-mode)

;; coffee-mode: compile to JavaScript at save file
(add-hook 'coffee-mode-hook 'coffee-cos-mode)

;; less-css-mode: compile to CSS at save file
(setq less-css-compile-at-save t)

;; web-mode: extra file associations
(add-to-list 'auto-mode-alist '("\.as[cp]x$" . web-mode))
(add-to-list 'auto-mode-alist '("\.config$" . web-mode))
(add-to-list 'auto-mode-alist '("\.master$" . web-mode))
(add-to-list 'auto-mode-alist '("\.php$" . web-mode))
(add-to-list 'auto-mode-alist '("\.phtml$" . web-mode))
(add-to-list 'auto-mode-alist '("\.tpl\.php$" . web-mode))
(add-to-list 'auto-mode-alist '("\.jsp$" . web-mode))
(add-to-list 'auto-mode-alist '("\.erb$" . web-mode))
(add-to-list 'auto-mode-alist '("\.mustache$" . web-mode))
(add-to-list 'auto-mode-alist '("\.djhtml$" . web-mode))

;; csharp-mode: extra file associations
;;   *.asmx: ASP.NET Web Services source file
(setq auto-mode-alist (cons '("\.asmx$" . csharp-mode) auto-mode-alist))

;; markdown-mode: extra file associations
;;   *.ronn: https://github.com/rtomayko/ronn
(add-to-list 'auto-mode-alist '("\.ronn$" . markdown-mode))



;; More
;; ----

(load "~/.emacs.d/init-misc.el")
(load "~/.emacs.d/init-ibus.el")

;; elscreen has to precede powerline, due to a clash between them
(load "~/.emacs.d/init-elscreen.el")
(load "~/.emacs.d/init-powerline.el")

;; Restore cursor color (dirty hack)
(set-cursor-color "#107f10")
