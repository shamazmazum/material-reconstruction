(in-package :material-reconstruction)

;; Generic correlation function class
(defclass corrfn () ()
  (:documentation "Generic correlation function class"))

;; Separate s2 correlation function class
(defclass corrfn-s2 (corrfn gpu-object)
  ()
  (:documentation "Class for S₂ (two-point) correlation functions
without association with an image. @c(:context) keyword argument must
hold OpenCL context. The context must remain alive while a created
image lives. @c(:s2) keyword must hold S₂ correlation function and
@c(:dimensions) are dimensions of the original image."))

(defclass corrfn-l2 (corrfn)
  ((l2-void  :initarg  :l2-void
             :accessor l2-void
             :type     list)
   (l2-solid :initarg  :l2-solid
             :accessor l2-solid
             :type     list))
  (:documentation "Lineal-path correlation function for solid and void phases."))

(defmethod initialize-instance :after ((corrfn-s2 corrfn-s2)
                                       &key context s2 dimensions &allow-other-keys)
  (setf (object-sap corrfn-s2)
        (%create-corrfn
         (object-sap context)
         s2 dimensions)))

(defmethod destroy-gpu-object ((corrfn-s2 corrfn-s2))
  ;; These objects are an_image on C side
  (%destroy-image (object-sap corrfn-s2)))
