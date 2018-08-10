(define (to-png pattern)
  (let* ((filelist (cadr (file-glob pattern 1))))
    (while (not (null? filelist))
           (let* ((filename (car filelist))
                  (new-filename (string-append filename ".png"))
                  (image (car (gimp-file-load RUN-NONINTERACTIVE
                                              filename
                                              filename)))
                  (drawable (car (gimp-image-get-active-layer image))))
             ;(gimp-levels-stretch drawable)
             (gimp-file-save RUN-NONINTERACTIVE image
                             drawable
                             new-filename
                             new-filename)
             (gimp-image-delete image))
           (set! filelist (cdr filelist)))))
