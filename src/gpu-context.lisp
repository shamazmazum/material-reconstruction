(in-package :material-reconstruction)

(defun read-kernel (kernel)
  (with-output-to-string (output)
    (with-open-file (input kernel)
      (loop for line = (read-line input nil)
            while line do
              (write-line line output)))))

(defclass gpu-context (gpu-object)
  ((dimensions          :type           (integer 1 3)
                        :initarg        :dimensions
                        :initform       (error "Specify dimensionality")
                        :reader         gpu-context-dimensions
                        :documentation "Dimensionality of all images
created with this context.")
   (validation-layers-p :type           boolean
                        :initarg        :validation-layers-p
                        :initform       nil
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
