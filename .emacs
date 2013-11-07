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

;; Set window transparency
(set-frame-parameter (selected-frame) 'alpha '(88 88))
(add-to-list 'default-frame-alist '(alpha 88 88))

;; Hide the tool-bar
(tool-bar-mode -1)
;; Hide the menu-bar
(menu-bar-mode -1)
;; Hide the tooltip
(tooltip-mode -1)
;; Show line number
(global-linum-mode t)

;; Set frame title
(setq frame-title-format "%b")

;; Set default font family / size
(set-default-font "Inconsolata 13")

;; Set basic colors
(set-background-color "#000f00")
(set-face-background 'region "#103f10")
(set-cursor-color "#107f10")

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

;; Common User Access (CUA) mode
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

;; Use K&R style
(setq c-default-style
      '((java-mode . "java")
	(awk-mode . "awk")
	(other . "k&r")))

;; Scrolling down the view (1 line)
(global-set-key (kbd "M-n") (lambda () (interactive) (scroll-up 1)))
(eval-after-load 'markdown-mode
  '(define-key markdown-mode-map
     (kbd "M-n") (lambda () (interactive) (scroll-up 1))))

;; Scrolling down the view
(global-set-key (kbd "M-<down>") (lambda () (interactive) (scroll-up 4)))
(eval-after-load 'markdown-mode
  '(define-key markdown-mode-map
     (kbd "M-<down>") (lambda () (interactive) (scroll-up 4))))

;; Scrolling up the view (1 line)
(global-set-key (kbd "M-p") (lambda () (interactive) (scroll-down 1)))
(eval-after-load 'markdown-mode
  '(define-key markdown-mode-map
      (kbd "M-p") (lambda () (interactive) (scroll-down 1))))

;; Scrolling up the view
(global-set-key (kbd "M-<up>") (lambda () (interactive) (scroll-down 4)))
(eval-after-load 'markdown-mode
  '(define-key markdown-mode-map
     (kbd "M-<up>") (lambda () (interactive) (scroll-down 4))))

;; Enable mouse scroll-wheel scaling
(global-set-key (kbd "<C-mouse-4>") 'text-scale-increase)
(global-set-key (kbd "<C-mouse-5>") 'text-scale-decrease)

;; Use /bin/zsh as explicit shell
(setq explicit-shell-file-name "/bin/zsh")

;; Enable ElScreen
;; [AUR] emacs-elscreen-emacs24-git
(load "elscreen" "ElScreen" t)
(elscreen-start)
(global-unset-key "\M-s")
(elscreen-set-prefix-key "\M-s")
(setq elscreen-tab-display-kill-screen nil)
(global-set-key (kbd "<C-tab>") 'elscreen-next)
(global-set-key (kbd "<header-line> <mouse-5>") 'elscreen-next)
(global-set-key (kbd "<C-S-iso-lefttab>") 'elscreen-previous)
(global-set-key (kbd "<header-line> <mouse-4>") 'elscreen-previous)

;; M-s SPC
;; Open all buffers in individual screens
(defun all-buffers-elscreen ()
  (interactive)
  (setq temp-list (buffer-list))
  (while temp-list
    (setq file-name (buffer-file-name (car temp-list)))
    (if file-name
        (elscreen-find-file file-name))
    (setq temp-list (cdr temp-list))))
(global-set-key (kbd "M-s SPC") 'all-buffers-elscreen)

;; Location for saving Elscreen tabs / desktop sessions
(setq pwd (concat (getenv "PWD") "/"))
(setq desktop-dirname pwd
      desktop-path (list desktop-dirname)
      desktop-save t)
(desktop-save-mode 0)
(defvar emacs-configuration-directory
    pwd
    "The directory where the emacs configuration files are stored.")
(defvar elscreen-tab-configuration-store-filename
    (concat emacs-configuration-directory ".emacs.elscreen")
    "The file where the elscreen tab configuration is stored.")

;; Store Elscreen tabs / desktop sessions
(defun elscreen-store ()
  "Store the elscreen tab configuration."
  (interactive)
  (if (desktop-save emacs-configuration-directory)
      (with-temp-file elscreen-tab-configuration-store-filename
        (insert (prin1-to-string (elscreen-get-screen-to-name-alist))))))
(global-set-key (kbd "M-s r") 'elscreen-store)

;; Restore Elscreen tabs / desktop sessions
(defun elscreen-restore ()
  "Restore the elscreen tab configuration."
  (interactive)
  (if (eq (type-of (desktop-read)) 'symbol)
      (let ((screens (reverse
                      (read
                       (with-temp-buffer
                         (insert-file-contents elscreen-tab-configuration-store-filename)
                         (buffer-string))))))
        (while screens
          (setq screen (car (car screens)))
          (setq buffers (split-string (cdr (car screens)) ":"))
          (if (eq screen 0)
              (switch-to-buffer (car buffers))
            (elscreen-find-and-goto-by-buffer (car buffers) t t))
          (while (cdr buffers)
            (switch-to-buffer-other-window (car (cdr buffers)))
            (setq buffers (cdr buffers)))
          (setq screens (cdr screens))))))

;; Enable ElScreen-dnd
;; With ElScreen-dnd, new screens are automatically created for each file drag&dropped
(load "elscreen-dnd")
(setq elscreen-dnp-drag-n-drop t)

;; Enable Xwidget
(require 'xwidget)

;; Load Powerline
;; [AUR] emacs-powerline-git
(require 'powerline)
(powerline-default-theme)
(custom-set-faces
 '(mode-line ((t (:foreground "#030303" :background "#bdbdbd" :box nil))))
 '(mode-line-inactive ((t (:foreground "#f9f9f9" :background "#666666" :box nil)))))

;; Load MELPA
(require 'package)
(add-to-list 'package-archives
  '("melpa" . "http://melpa.milkbox.net/packages/") t)

;; Load Haskell Mode
;; [Arch] emacs-haskell-mode
(require 'haskell-mode-autoloads)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)

;; Load Lua Mode
;; [Arch] emacs-lua-mode
(autoload 'lua-mode "lua-mode" "Lua editing mode." t)
(setq auto-mode-alist (cons '("\.lua$" . lua-mode) auto-mode-alist))

;; Load PKGBUILD Mode
;; [Arch] emacs-pkgbuild-mode
(autoload 'pkgbuild-mode "pkgbuild-mode.el" "PKGBUILD mode." t)
(setq auto-mode-alist (append '(("/PKGBUILD$" . pkgbuild-mode)) auto-mode-alist))
(add-to-list 'auto-mode-alist '("\.install$" . sh-mode))

;; Load Python Mode
;; [Arch] emacs-python-mode
(autoload 'python-mode "python-mode.el" "Python mode." t)
(setq auto-mode-alist (append '(("/*.\.py$" . python-mode)) auto-mode-alist))

;; Load Android Mode
;; [AUR] emacs-android-git
(require 'android-mode)
(setq android-mode-sdk-dir "/opt/android-sdk/")
(setq android-mode-avd "My_Galaxy_Nexus")

;; Load Arduino Mode
;; [AUR] emacs-arduino-mode-git
(autoload 'arduino-mode "arduino-mode" "Arduino editing mode." t)
(add-to-list 'auto-mode-alist '("\.ino$" . arduino-mode))

;; Load Clojure Mode
;; [AUR] emacs-clojure-mode-git
(require 'clojure-mode)

;; Load Emmet Mode
;; [AUR] emacs-emmet-mode-git
(require 'emmet-mode)
(add-hook 'sgml-mode-hook 'emmet-mode)
(add-hook 'css-mode-hook  'emmet-mode)

;; Load Groovy Mode
;; [AUR] emacs-groovy-mode
(add-to-list 'load-path "/usr/share/emacs/site-lisp/groovy-mode")
;;; turn on syntax highlighting
(global-font-lock-mode 1)
;;; use groovy-mode when file ends in .groovy or has #!/bin/groovy at start
(autoload 'groovy-mode "groovy-mode" "Major mode for editing Groovy code." t)
(add-to-list 'auto-mode-alist '("\.groovy$" . groovy-mode))
(add-to-list 'interpreter-mode-alist '("groovy" . groovy-mode))
;;; make Groovy mode electric by default.
(add-hook 'groovy-mode-hook
          '(lambda ()
             (require 'groovy-electric)
             (groovy-electric-mode)))

;; Load Idris Mode
;; [AUR] emacs-idris-mode-git
(require 'idris-mode)

;; Load Markdown Mode
;; [AUR] emacs-markdown-mode
(autoload 'markdown-mode "markdown-mode" "Markdown mode" t)
(setq auto-mode-alist (cons '("\.md$" . markdown-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\.markdown$" . markdown-mode) auto-mode-alist))

;; Load SML Mode
;; [AUR] emacs-sml-mode
(require 'sml-mode)

;; Load Whitespace Mode
;; [AUR] emacs-whitespace-mode
(autoload 'whitespace-mode "whitespace-mode.el" "Whitespace editing mode." t)
(setq auto-mode-alist (cons '("\.ws$" . whitespace-mode) auto-mode-alist))

;; Load YAML Mode
;; [AUR] emacs-yaml-mode
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\.yml\'" . yaml-mode))

;; Load Gem Specification files (*.gemspec) in ruby-mode
(add-to-list 'auto-mode-alist '("\.gemspec$" . ruby-mode))

;; Load ronn files (*.ronn) in markdown-mode
(add-to-list 'auto-mode-alist '("\.ronn$" . markdown-mode))

;; Redefine android-start-emulator to use KVM and GPU acceleration
(defun android-start-emulator ()
  "Launch Android emulator."
  (interactive)
  (let ((avd (or (and (not (string= android-mode-avd "")) android-mode-avd)
                 (completing-read "Android Virtual Device: " (android-list-avd)))))
    (unless (android-start-exclusive-command (concat "*android-emulator-" avd "*")
                                             (concat (android-tool-path "emulator") " -avd " avd " -gpu on -qemu -m 512 -enable-kvm"))
      (message (concat "emulator " avd " already running")))))

;; Processing configuration
(setq processing-location "/usr/bin/processing-java")
(setq processing-sketchbook-dir "~/sketch")

;; Automatic store
(if (and (= 1 (length command-line-args))
         (file-exists-p elscreen-tab-configuration-store-filename))
    (push #'elscreen-store kill-emacs-hook))

;; Automatic restore
;; Must be put at last so that all necessary modes load
(if (and (= 1 (length command-line-args))
         (file-readable-p elscreen-tab-configuration-store-filename))
    (elscreen-restore))
