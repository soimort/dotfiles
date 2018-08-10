(define (to-srgb-png pattern)
  (let* ((filelist (cadr (file-glob pattern 1))))
    (while (not (null? filelist))
           (let* ((filename (car filelist))
                  (new-filename (string-append filename "_srgb.png"))
                  (image (car (gimp-file-load RUN-NONINTERACTIVE
                                              filename
                                              filename)))
                  (drawable (car (gimp-image-get-active-layer image))))
             (plug-in-icc-profile-apply-rgb RUN-NONINTERACTIVE image 0 0)
             (gimp-file-save RUN-NONINTERACTIVE image
                             drawable
                             new-filename
                             new-filename)
             (gimp-image-delete image))
           (set! filelist (cdr filelist)))))
