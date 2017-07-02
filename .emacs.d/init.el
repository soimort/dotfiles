;;; Emacs initialization file
;;; @prog         emacs
;;; @lastProgVers 24.5
;;; @since        2015-12-23
;;; @lastChanged  2016-04-22
;;; @author       Mort Yao <soi@mort.ninja>

;; Custom
;; ------

(setq custom-file "~/.emacs.d/custom.el")
(if (file-exists-p custom-file)
    (load custom-file))



;; User Interface
;; --------------

;; Minimize UI
(tool-bar-mode -1) ; hide tool-bar
(menu-bar-mode -1) ; hide menu-bar
(tooltip-mode  -1) ; hide tooltip

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
(set-default-font "Ubuntu Mono 12")

;; Show line numbers
(global-linum-mode t)

;; Highlight current line
(global-hl-line-mode 1)
(set-face-background 'hl-line "#101f10")
(set-face-foreground 'highlight nil)

;; Show paren mode
(show-paren-mode 1)
;; Highlight parentheses
(set-face-background 'show-paren-match-face (face-background 'default))
(set-face-foreground 'show-paren-match-face "#ffff00")
(set-face-attribute 'show-paren-match-face nil :weight 'extra-bold)

;; [M-n] Scrolling down the view (by 1 line)
(global-set-key (kbd "M-n") (lambda () (interactive) (scroll-up 1)))
;; [M-<down>] Scrolling down the view
(global-set-key (kbd "M-<down>") (lambda () (interactive) (scroll-up 4)))
;; [M-p] Scrolling up the view (by 1 line)
(global-set-key (kbd "M-p") (lambda () (interactive) (scroll-down 1)))
;; [M-<up>] Scrolling up the view
(global-set-key (kbd "M-<up>") (lambda () (interactive) (scroll-down 4)))

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

;; Copy entire buffer
(defun copy-all ()
    "Copy entire buffer to clipboard"
    (interactive)
    (clipboard-kill-ring-save (point-min) (point-max))
    (message "Current buffer copied"))
(global-set-key (kbd "C-c c") 'copy-all)

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
;;(unless package-archive-contents (package-refresh-contents))
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
(c-set-offset 'innamespace 0)

;; JavaScript
(setq js-indent-level 2)
;; CSS
(setq css-indent-offset 2)



;; More
;; ----

(when (require 'elscreen nil 'noerror)
  (load "~/.emacs.d/init-elscreen.el"))

(when (require 'powerline nil 'noerror)
  (powerline-default-theme))

(setq init-extra "~/.emacs.d/init-extra.el")
(if (file-exists-p init-extra)
    (condition-case nil
        (load init-extra)
      (error nil)))
