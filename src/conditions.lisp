(in-package :material-reconstruction)

(define-condition recon-error (simple-error)
  ()
  (:documentation "Generic material reconstruction error"))

(define-condition recon-warning (simple-warning)
  ()
  (:documentation "Generic material reconstruction warning"))
