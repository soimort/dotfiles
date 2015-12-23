
;; text-mode
(add-hook 'text-mode-hook 'flyspell-mode)

;; markdown-mode
(add-hook 'markdown-mode-hook 'flyspell-mode)
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

;; web-mode
(setq web-mode-code-indent-offset 2)
(setq web-mode-css-indent-offset 2)
(setq web-mode-markup-indent-offset 2)

;; emmet-mode
(add-hook 'sgml-mode-hook 'emmet-mode)
(add-hook 'css-mode-hook  'emmet-mode)

;; coffee-mode
(setq coffee-tab-width 2)
(add-hook 'coffee-mode-hook 'coffee-cos-mode)

;; haskell-mode
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
