(in-package :material-reconstruction)

(declaim (type boolean *validation-layers-p*))
(defparameter *validation-layers-p* nil
  "Should validation layers be enabled by default?")

(defclass gpu-context (gpu-object)
  ((dimensions          :type           (integer 1 3)
                        :initarg        :dimensions
                        :initform       (error "Specify dimensionality")
                        :reader         gpu-context-dimensions
                        :documentation "Dimensionality of all images
created with this context.")
   (validation-layers-p :type           boolean
                        :initarg        :validation-layers-p
                        :initform       *validation-layers-p*
                        :reader         gpu-context-validation-layers-p
                        :documentation "Do we require validation
layers when creating a context?"))
  (:documentation "Class representing an OpenCL context"))

(defmethod initialize-instance :after ((gpu-context gpu-context) &rest initargs)
  (declare (ignore initargs))
  (setf (object-sap gpu-context)
        (%create-gpu-context
         (gpu-context-dimensions gpu-context)
         (gpu-context-validation-layers-p gpu-context))))

(defmethod destroy-gpu-object ((gpu-context gpu-context))
  (%destroy-gpu-context (object-sap gpu-context)))
