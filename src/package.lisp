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

           #:destroy-gpu-object
           #:with-gpu-object
           #:with-gpu-objects

           #:gpu-context

           #:image
           #:image-s2
           #:image-l2
           #:image-all
           #:image-pixel
           #:image-dimensions
           #:image-array

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
