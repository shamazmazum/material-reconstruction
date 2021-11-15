(defpackage material-reconstruction
  (:nicknames :recon)
  (:use #:cl
        #:cffi
        #:array-operations/utilities)
  (:import-from :alexandria
                #:with-gensyms
                #:non-negative-fixnum
                #:positive-fixnum)
  (:import-from :serapeum #:->)
  (:export #:recon-error
           #:recon-warning

           ;; Binding to FFTW3
           #:rfft 

           ;; Correlation functions
           #:l2
           #:s2

           #:destroy-gpu-object
           #:with-gpu-object
           #:with-gpu-objects

           #:gpu-context

           #:image
           #:image-s2
           #:image-l2
           #:image-pixel
           #:image-dimensions
           #:image-array

           #:corrfn
           #:corrfn-s2
           #:corrfn-l2

           #:proximeter
           #:proximity

           #:modifier
           #:sampler
           #:flipper
           #:swapper
           #:batch-modifier
           #:interface-sampler
           #:uniform-sampler

           #:initialize-random

           #:exponential-cooldown
           #:aarts-korst-cooldown

           #:cost
           #:cost-state
           #:annealing-step
           #:run-annealing))
