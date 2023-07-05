(defpackage material-reconstruction
  (:nicknames :recon)
  (:use #:cl
        #:cffi
        #:array-operations/utilities)
  (:local-nicknames (#:si   #:stateless-iterators)
                    (#:alex #:alexandria)
                    (#:sera #:serapeum))
  (:import-from :alexandria
                #:with-gensyms
                #:non-negative-fixnum
                #:positive-fixnum)
  (:import-from :serapeum #:->)
  (:export #:recon-error
           #:recon-warning

           ;; Correlation functions
           #:s2

           #:destroy-gpu-object
           #:with-gpu-object
           #:with-gpu-objects

           #:gpu-context

           #:image
           #:update-callback-mixin
           #:image-s2
           #:image-pixel
           #:image-dimensions
           #:image-total-size
           #:image-array
           #:image-gpu-s2

           #:corrfn
           #:corrfn-s2

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
           #:neighbors-map
           #:neighbors-hist

           #:initialize-random

           #:exponential-cooldown
           #:aarts-korst-cooldown

           #:cost
           #:cost-state
           #:annealing-step
           #:run-annealing))
