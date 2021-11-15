(in-package :material-reconstruction)

(defclass proximeter (gpu-object)
  ((image-x :initarg :image-x
            :type    image-s2
            :reader  proximeter-image-x)
   (image-y :initarg :image-y
            :type    image-s2
            :reader  proximeter-image-y))
  (:documentation "Proximeter is a handle which must be used to get a
distance between two images according to two-point correlation
functon."))

(defmethod initialize-instance :after ((proximeter proximeter) &rest initargs)
  (declare (ignore initargs))
  (setf (object-sap proximeter)
        (%create-proximeter
         (object-sap (proximeter-image-x proximeter))
         (object-sap (proximeter-image-y proximeter)))))

(defmethod destroy-gpu-object ((proximeter proximeter))
  (%destroy-proximeter (object-sap proximeter)))

(-> proximity (proximeter) (values double-float &optional))
(defun proximity (proximeter)
  "Return a distance (a double-precision floating point value) between
tracked images according to two-point correlation function."
  (%proximity (object-sap proximeter)))
