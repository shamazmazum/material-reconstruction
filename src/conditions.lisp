(in-package :material-reconstruction)

(define-condition recon-error (error)
  ()
  (:documentation "Generic material reconstruction error"))

(define-condition gpu-context-error (recon-error)
  ()
  (:documentation "Signalled on GPU error"))
