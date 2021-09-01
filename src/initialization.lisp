(in-package :material-reconstruction)

(defun count-phase (image phase)
  (loop for y below (image-height image)
        sum (loop for x below (image-width image)
                  sum (if (= phase (image-get image x y)) 1 0))))

;; This is generally for tests
(defun initialize-random (image target &key (phase 1))
  (when (or (/= (image-width image)
                (image-width target))
            (/= (image-height image)
                (image-height target)))
    (error 'recon-simple-error
           :format-control "Dimension mismatch"))

  (loop with phase-image    = (count-phase image  phase)
        with phase-target   = (count-phase target phase)
        with width          = (image-width  image)
        with height         = (image-height image)
        while (< phase-image phase-target) do
          (let ((x (random width))
                (y (random height)))
            (when (/= (image-get image x y) phase)
              (incf phase-image)
              (image-set image x y phase))))
  (image-fft image))
