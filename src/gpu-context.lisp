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

(defstruct (gpu-context (:conc-name nil))
  context-sap)

(defun create-gpu-context ()
  (make-gpu-context
   :context-sap (%create-gpu-context (read-kernel +kernel+))))

(defun destroy-gpu-context (context)
  (%destroy-gpu-context (context-sap context)))

(defmacro with-gpu-context ((context) &body body)
  `(let ((,context (create-gpu-context)))                    
     (unwind-protect
          (progn ,@body)
       (destroy-gpu-context ,context))))
