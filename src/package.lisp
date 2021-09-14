(defpackage material-reconstruction
  (:nicknames :recon)
  (:use #:cl
        #:cffi
        #:array-operations/utilities)
  (:export #:recon-error
           #:gpu-context-error

           #:gpu-context
           #:create-gpu-context
           #:destroy-gpu-context
           #:with-gpu-context

           #:image
           #:create-image
           #:destroy-image
           #:load-image
           #:image-get
           #:image-set
           #:image-width
           #:image-height
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
           #:batch-modifier
           #:interface-sampler
           #:uniform-sampler

           #:initialize-random

           #:exponential-cooldown
           #:aarts-korst-cooldown

           #:s2-cost
           #:annealing-step
           #:run-annealing
           #:image->array))
