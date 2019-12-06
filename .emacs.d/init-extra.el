
;; python.el
(add-hook 'python-mode-hook
          (lambda ()
            (setq indent-tabs-mode nil
                  python-indent 4
                  python-indent-offset 4)))
(with-eval-after-load 'python
  (define-key python-mode-map (kbd "<tab>") 'python-indent-shift-right)
  (define-key python-mode-map (kbd "<S-iso-lefttab>") 'python-indent-shift-left) ; <S-tab> or <backtab> not work
  )

;; asm-mode
(defun my-asm-mode-hook ()
  ;; you can use `comment-dwim' (M-;) for this kind of behaviour anyway
  (local-unset-key (vector asm-comment-char))
  ;; asm-mode sets it locally to nil, to "stay closer to the old TAB behaviour"
  (setq tab-always-indent (default-value 'tab-always-indent)))
(add-hook 'asm-mode-hook #'my-asm-mode-hook)

;; nasm-mode
(add-to-list 'auto-mode-alist '("\\.asm?\\'" . nasm-mode))

;; shell-script-mode
(add-to-list 'auto-mode-alist '("\\.install\\'" . shell-script-mode))

;; text-mode
(add-hook 'text-mode-hook 'flyspell-mode)

;; markdown-mode [elpa]
(add-hook 'markdown-mode-hook 'flyspell-mode)
(setq markdown-command "pandoc")
(setq markdown-enable-math t)
(add-to-list 'auto-mode-alist '("\\.md.txt$" . markdown-mode))
;; fix custom keybinding
(eval-after-load 'markdown-mode
  '(define-key markdown-mode-map
     (kbd "M-n") (lambda () (interactive) (scroll-up 1))))
(eval-after-load 'markdown-mode
  '(define-key markdown-mode-map
     (kbd "M-<down>") (lambda () (interactive) (scroll-up 4))))
(eval-after-load 'markdown-mode
  '(define-key markdown-mode-map
     (kbd "M-p") (lambda () (interactive) (scroll-down 1))))
(eval-after-load 'markdown-mode
  '(define-key markdown-mode-map
     (kbd "M-<up>") (lambda () (interactive) (scroll-down 4))))

;; web-mode [elpa]
(setq web-mode-code-indent-offset 2)
(setq web-mode-css-indent-offset 2)
(setq web-mode-markup-indent-offset 2)
(setq web-mode-enable-current-column-highlight t)
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.hbs\\'" . web-mode))

;; emmet-mode [elpa]
(add-hook 'sgml-mode-hook 'emmet-mode)
(add-hook 'css-mode-hook  'emmet-mode)
(add-hook 'web-mode-hook  'emmet-mode)

;; coffee-mode [elpa]
(setq coffee-tab-width 2)
(add-hook 'coffee-mode-hook 'coffee-cos-mode) ; compile on save

;; less-css-mode [elpa]
(setq less-css-compile-at-save t) ; compile on save

;; haskell-mode [elpa]
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)

;; clang-format [arch:clang]
(load "/usr/share/clang/clang-format.el")
(global-set-key (kbd "C-` TAB") 'clang-format-region)

;; maxima-mode [arch:maxima]
(autoload 'maxima-mode "maxima" "Maxima mode" t)
(autoload 'imaxima "imaxima" "Frontend for maxima with Image support" t)
(autoload 'maxima "maxima" "Maxima interaction" t)
(autoload 'imath-mode "imath" "Imath mode for math formula input" t)
(setq imaxima-use-maxima-mode-flag t)
(add-to-list 'auto-mode-alist '("\\.ma[cx]$" . maxima-mode))

;; mozart/oz [aur:mozart2]
;(setq load-path (cons "/usr/share/mozart/elisp/" load-path))
                                        ;(require 'oz)

;; twelf-mode [aur:twelf]
;(setq twelf-root "/opt/twelf/")
;(load (concat twelf-root "emacs/twelf-init.el"))

;; coq-mode [arch:coq]
;; <https://coq.inria.fr/distrib/current/refman/tools.html#Emacs>
;(setq auto-mode-alist (cons '("\\.v$" . coq-mode) auto-mode-alist))
;(autoload 'coq-mode "gallina" "Major mode for editing Coq vernacular." t)

;; proof-general [aur:proofgeneral] [elpa]
(condition-case nil
    (load "/usr/share/emacs/site-lisp/ProofGeneral/generic/proof-site")
  (error nil))
(add-hook 'proof-mode-hook 'flyspell-prog-mode)
;; fix custom keybinding
(eval-after-load 'coq
  '(progn
     (define-key proof-mode-map
       (kbd "M-n") (lambda () (interactive) (scroll-up 1)))
     (define-key proof-mode-map
       (kbd "M-<down>") (lambda () (interactive) (scroll-up 4)))
     (define-key proof-mode-map
       (kbd "M-p") (lambda () (interactive) (scroll-down 1)))
     (define-key proof-mode-map
       (kbd "M-<up>") (lambda () (interactive) (scroll-down 4)))))

;; hol-mode [aur:hol]
(autoload 'hol "/opt/hol/tools/hol-mode"
  "Runs a HOL session in a comint window.
   With a numeric prefix argument, runs it niced to that level
   or at level 10 with a bare prefix. " t)
;(load "/opt/hol/tools/hol-mode")

;; agda-mode [arch:agda]
(load-file (let ((coding-system-for-read 'utf-8))
                (shell-command-to-string "agda-mode locate")))

;; agda-mode theme for dark background
;; https://www.reddit.com/r/agda/comments/5pwip4/show_off_your_agda_emacs_colours/
(let ((base03    "#002b36")
      (base02    "#073642")
      (base01    "#586e75")
      (base00    "#657b83")
      (base0     "#839496")
      (base1     "#93a1a1")
      (base2     "#eee8d5")
      (base3     "#fdf6e3")
      (yellow    "#b58900")
      (orange    "#cb4b16")
      (red       "#dc322f")
      (magenta   "#d33682")
      (violet    "#6c71c4")
      (blue      "#268bd2")
      (cyan      "#2aa198")
      (green     "#859900"))
  (custom-set-faces
   `(agda2-highlight-keyword-face ((t (:foreground ,orange))))
   `(agda2-highlight-string-face ((t (:foreground ,magenta))))
   `(agda2-highlight-number-face ((t (:foreground ,violet))))
   `(agda2-highlight-symbol-face ((((background ,base3)) (:foreground ,base01))))
   `(agda2-highlight-primitive-type-face ((t (:foreground ,blue))))
   `(agda2-highlight-bound-variable-face ((t nil)))
   `(agda2-highlight-inductive-constructor-face ((t (:foreground ,green))))
   `(agda2-highlight-coinductive-constructor-face ((t (:foreground ,yellow))))
   `(agda2-highlight-datatype-face ((t (:foreground ,blue))))
   `(agda2-highlight-field-face ((t (:foreground ,red))))
   `(agda2-highlight-function-face ((t (:foreground ,blue))))
   `(agda2-highlight-module-face ((t (:foreground ,violet))))
   `(agda2-highlight-postulate-face ((t (:foreground ,blue))))
   `(agda2-highlight-primitive-face ((t (:foreground ,blue))))
   `(agda2-highlight-record-face ((t (:foreground ,blue))))
   `(agda2-highlight-dotted-face ((t nil)))
   `(agda2-highlight-operator-face ((t nil)))
   `(agda2-highlight-error-face ((t (:foreground ,red :underline t))))
   `(agda2-highlight-unsolved-meta-face ((t (:background ,base03 :foreground ,yellow))))
   `(agda2-highlight-unsolved-constraint-face ((t (:background ,base03 :foreground ,yellow))))
   `(agda2-highlight-termination-problem-face ((t (:background ,orange :foreground ,base03))))
   `(agda2-highlight-incomplete-pattern-face ((t (:background ,orange :foreground ,base03))))
   `(agda2-highlight-typechecks-face ((t (:background ,cyan :foreground ,base03))))))
