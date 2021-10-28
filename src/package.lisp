(defpackage material-reconstruction
  (:nicknames :recon)
  (:use #:cl
        #:cffi
        #:array-operations/utilities
        #:select)
  (:export #:recon-error
           #:gpu-context-error

           #:gpu-context
           #:create-gpu-context
           #:destroy-gpu-context
           #:with-gpu-context

           #:image
           #:image-s2
           #:image-l2
           #:image-all
           #:create-image
           #:destroy-image
           #:image-pixel
           #:image-dimensions
           #:image-array
           #:with-image
           #:with-images

           #:proximeter
           #:create-proximeter
           #:destroy-proximeter
           #:with-proximeter
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

           #:s2-cost
           #:annealing-step
           #:run-annealing))
