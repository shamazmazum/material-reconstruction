(in-package :material-reconstruction)

;; Generic correlation function class
(defclass corrfn () ()
  (:documentation "Generic correlation function class"))

;; Separate s2 correlation function class
(defclass corrfn-s2 (corrfn gpu-object)
  ()
  (:documentation "Class for S₂ (two-point) correlation functions
without association with an image. When instantiating, @c(:context)
keyword argument must hold @c(gpu-context) object. The context must
remain alive while the created image lives. @c(:s2) keyword must hold
S₂ correlation function and @c(:dimensions) are dimensions of the
original image. Alternatively, you can pass @c(:array) and
@c(:periodic-p) arguments to calculate S₂ function from that array."))

(defmethod initialize-instance :after ((corrfn-s2 corrfn-s2)
                                       &key (periodic-p t) context array s2 dimensions
                                       &allow-other-keys)
  (let* ((array      (if array (funcall (if periodic-p #'identity #'pad-with-zeros) array)))
         (s2         (if array (s2-dft array) s2))
         (dimensions (if array (array-dimensions array) dimensions)))
    (setf (object-sap corrfn-s2)
          (%create-corrfn
           (object-sap context)
           s2 dimensions))))

(defmethod destroy-gpu-object ((corrfn-s2 corrfn-s2))
  ;; These objects are an_corrfn on C side
  (%destroy-corrfn (object-sap corrfn-s2)))
