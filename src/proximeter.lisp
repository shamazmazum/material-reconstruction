(in-package :material-reconstruction)

(defclass proximeter (gpu-object)
  ((target :initarg :target
           :type    corrfn-s2
           :reader  proximeter-target)
   (recon  :initarg :recon
           :type    image-s2
           :reader  proximeter-recon))
  (:documentation "Proximeter is a handle which must be used to get a
distance between two images according to two-point correlation
functon."))

(defmethod initialize-instance :after ((proximeter proximeter) &rest initargs)
  (declare (ignore initargs))
  (setf (object-sap proximeter)
        (%create-proximeter
         (object-sap (proximeter-target proximeter))
         (object-sap (proximeter-recon  proximeter)))))

(defmethod destroy-gpu-object ((proximeter proximeter))
  (%destroy-proximeter (object-sap proximeter)))

(-> proximity (proximeter) (values double-float &optional))
(defun proximity (proximeter)
  "Return a distance (a double-precision floating point value) between
the tracked image and target two-point correlation function."
  (%proximity (object-sap proximeter)))
