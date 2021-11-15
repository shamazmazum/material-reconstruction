(in-package :material-reconstruction)

(defconstant +kernel+
  (asdf:system-relative-pathname '#:material-reconstruction
                                 "src/gpu.cl"))

(defun read-kernel (kernel)
  (with-output-to-string (output)
    (with-open-file (input kernel)
      (loop for line = (read-line input nil)
            while line do
              (write-line line output)))))

(defclass gpu-context (gpu-object)
  ()
  (:documentation "Class representing an OpenCL context"))

(defmethod initialize-instance :after ((gpu-context gpu-context) &rest initargs)
  (declare (ignore initargs))
  (setf (object-sap gpu-context)
        (%create-gpu-context (read-kernel +kernel+))))

(defmethod destroy-gpu-object ((gpu-context gpu-context))
  (%destroy-gpu-context (object-sap gpu-context)))
