
;; Enable elscreen
(load "elscreen" "ElScreen" t)
(elscreen-start)
(setq elscreen-tab-display-kill-screen nil)

;; Key binding
(global-unset-key "\M-s")
(elscreen-set-prefix-key "\M-s")
(global-set-key (kbd "<C-tab>") 'elscreen-next)
(global-set-key (kbd "<header-line> <mouse-5>") 'elscreen-next)
(global-set-key (kbd "<C-S-iso-lefttab>") 'elscreen-previous)
(global-set-key (kbd "<header-line> <mouse-4>") 'elscreen-previous)
(global-set-key (kbd "M-s <backspace>") 'elscreen-swap)
(global-set-key (kbd "M-s f") 'elscreen-find-file)
(global-set-key (kbd "C-` f") 'elscreen-find-file)

;; [M-s <left>] Move the current screen to the left. (by swapping with the previous)
(defun elscreen-move-left()
  "Move the current screen to the left."
  (interactive)
  (if (= (elscreen-get-current-screen) 0)
      (elscreen-message "Can't move the leftmost screen to the left.")
    (elscreen-previous)
    (elscreen-swap)))
(global-set-key (kbd "M-s <left>") 'elscreen-move-left)

;; [M-s <right>] Move the current screen to the right. (by swapping with the next)
(defun elscreen-move-right()
  "Move the current screen to the right."
  (interactive)
  (if (= (elscreen-get-current-screen) (- (elscreen-get-number-of-screens) 1))
      (elscreen-message "Can't move the rightmost screen to the right.")
    (elscreen-next)
    (elscreen-swap)))
(global-set-key (kbd "M-s <right>") 'elscreen-move-right)

;; [M-s SPC] Open all buffers in individual screens
(defun all-buffers-elscreen ()
  (interactive)
  (setq temp-list (buffer-list))
  (while temp-list
    (setq file-name (buffer-file-name (car temp-list)))
    (if file-name
        (elscreen-find-file file-name))
    (setq temp-list (cdr temp-list))))
(global-set-key (kbd "M-s SPC") 'all-buffers-elscreen)

;; [M-s h] Toggle window split
;; Vertical split shows more of each line, horizontal split shows more lines
(defun toggle-window-split ()
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
	     (next-win-buffer (window-buffer (next-window)))
	     (this-win-edges (window-edges (selected-window)))
	     (next-win-edges (window-edges (next-window)))
	     (this-win-2nd (not (and (<= (car this-win-edges)
					 (car next-win-edges))
				     (<= (cadr this-win-edges)
					 (cadr next-win-edges)))))
	     (splitter
	      (if (= (car this-win-edges)
		     (car (window-edges (next-window))))
		  'split-window-horizontally
		'split-window-vertically)))
	(delete-other-windows)
	(let ((first-win (selected-window)))
	  (funcall splitter)
	  (if this-win-2nd (other-window 1))
	  (set-window-buffer (selected-window) this-win-buffer)
	  (set-window-buffer (next-window) next-win-buffer)
	  (select-window first-win)
	  (if this-win-2nd (other-window 1))))))
(global-set-key (kbd "M-s h") 'toggle-window-split)

;; Location for saving elscreen tabs / desktop sessions
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

;; [M-s r] Store elscreen tabs / desktop sessions
(defun elscreen-store ()
  "Store the elscreen tab configuration."
  (interactive)
  (if (desktop-save emacs-configuration-directory)
      (with-temp-file elscreen-tab-configuration-store-filename
        (insert (prin1-to-string (elscreen-get-screen-to-name-alist))))))
(global-set-key (kbd "M-s r") 'elscreen-store)

;; Restore elscreen tabs / desktop sessions
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

;; Enable elscreen-dnd
;; With elscreen-dnd, new screens are automatically created for each file drag & dropped
(load "elscreen-dnd")
(setq elscreen-dnp-drag-n-drop t)

;; Automatic store
(if (and (= 1 (length command-line-args))
         (file-exists-p elscreen-tab-configuration-store-filename))
    (push #'elscreen-store kill-emacs-hook))

;; Automatic restore
;; Must be put at last so that all necessary modes load
(if (and (= 1 (length command-line-args))
         (file-readable-p elscreen-tab-configuration-store-filename))
    (elscreen-restore)
  (dired "."))
