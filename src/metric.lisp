(in-package :material-reconstruction)

(defclass metric-s2 (gpu-object)
  ()
  (:documentation "Objects of this class allow to compute difference
in S₂ (autocorrelation function) on GPU. Required keyword arguments to
@c(make-instance) are @c(context) for GPU context, @c(recon) for
reconstructed image and @c(target) for target autocorrelation
function."))

(defmethod initialize-instance :after ((metric-s2 metric-s2)
                                       &key context recon target &allow-other-keys)
  (setf (object-sap metric-s2)
        (%create-metric
         (object-sap context)
         (object-sap target)
         (object-sap recon))))

(defmethod destroy-gpu-object ((metric-s2 metric-s2))
  (%destroy-metric (object-sap metric-s2)))

(sera:-> metric-s2 (metric-s2)
         (values single-float &optional))
(defun metric-s2 (metric-s2)
  (%distance (object-sap metric-s2)))
                                                               
