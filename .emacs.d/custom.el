
(custom-set-variables

 '(custom-enabled-themes (quote (tango-dark)))

 '(inhibit-startup-screen t) ; show *scratch* buffer at startup
 ;; later: either restore elscreen sessions or enter dired-mode

 )

(custom-set-faces

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
   ((t (:background "#446644" :foreground "#ffffff" :inherit mode-line))))

 )
