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
(add-to-list 'auto-mode-alist '("\.html5$" . web-mode))
(setq web-mode-code-indent-offset 2)
(setq web-mode-css-indent-offset 2)
(setq web-mode-markup-indent-offset 2)

;; csharp-mode: extra file associations
;;   *.asmx: ASP.NET Web Services source file
(setq auto-mode-alist (cons '("\.asmx$" . csharp-mode) auto-mode-alist))

;; markdown-mode: extra file associations
;;   *.ronn: https://github.com/rtomayko/ronn
(add-to-list 'auto-mode-alist '("\.ronn$" . markdown-mode))
