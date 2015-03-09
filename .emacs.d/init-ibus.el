;; Load ibus.el
(add-to-list 'load-path "~/.emacs.d/ibus/")
(require 'ibus)
(add-hook 'after-init-hook 'ibus-mode-on)
