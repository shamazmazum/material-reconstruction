(defpackage material-reconstruction
  (:nicknames :recon)
  (:use #:cl
        #:cffi
        #:array-operations/utilities
        #:opticl)
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
           #:interface-sampler

           #:exponential-cooldown
           #:s2-cost
           #:annealing-step
           #:run-annealing))
