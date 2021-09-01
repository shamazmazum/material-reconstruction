(in-package :material-reconstruction)

(defconstant +kernel+
  (asdf:system-relative-pathname '#:material-reconstruction
                                 "src/gpu.cl"))

(defstruct (gpu-context (:conc-name nil))
  context-sap)

(defun create-gpu-context ()
  (make-gpu-context
   :context-sap (%create-gpu-context (namestring +kernel+))))

(defun destroy-gpu-context (context)
  (%destroy-gpu-context (context-sap context)))

(defmacro with-gpu-context ((context) &body body)
  `(let ((,context (create-gpu-context)))                    
     (unwind-protect
          (progn ,@body)
       (destroy-gpu-context ,context))))
