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
  (:local-nicknames (#:si #:stateless-iterators))
  (:export #:recon-error
           #:recon-warning

           ;; Correlation functions
           #:l2
           #:s2

           #:destroy-gpu-object
           #:with-gpu-object
           #:with-gpu-objects

           #:gpu-context

           #:image
           #:update-callback-mixin
           #:image-s2
           #:image-l2
           #:image-pixel
           #:image-dimensions
           #:image-array
           #:image-gpu-s2 ; For tests

           #:corrfn
           #:corrfn-s2
           #:corrfn-l2

           #:modifier
           #:sampler
           #:flipper
           #:swapper
           #:batch-modifier
           #:interface-sampler
           #:uniform-sampler
           #:dpn-sampler
           #:dpn-update-callback
           #:dpn-sampler-neighbors-map
           #:dpn-sampler-neighbors-hist

           #:initialize-random

           #:exponential-cooldown
           #:aarts-korst-cooldown

           #:cost
           #:cost-state
           #:annealing-step
           #:run-annealing))
