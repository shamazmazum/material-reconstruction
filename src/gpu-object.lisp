(in-package :material-reconstruction)

(defclass gpu-object ()
  ((sap :accessor object-sap))
  (:documentation "Generic GPU object"))

(defgeneric destroy-gpu-object (gpu-object)
  (:documentation "Free foreign GPU object"))

(defmethod destroy-gpu-object ((gpu-object gpu-object))
  (warn 'recon-warning
        :format-control "Object ~a is not destroyed"
        :format-arguments (list gpu-object)))

(defmacro with-gpu-object ((object class &rest make-instance-arguments) &body body)
  "Create GPU object @c(object) of class @c(class) and execute
@c(body) in its scope. The object is safely destroyed afterwards."
  `(let ((,object (make-instance ',class ,@make-instance-arguments)))
     (unwind-protect
          (progn ,@body)
       (destroy-gpu-object ,object))))

(defmacro with-gpu-objects (definitions &body body)
  "Create multiple GPU objects and execute @c(body) in their
scope. @c(definitions) must be a list of object definitions, each
definition must be in form used in @c(with-gpu-object)."
  (flet ((wrap-definition (definition forms)
           `(with-gpu-object ,definition ,forms)))
    (reduce #'wrap-definition definitions
            :from-end t
            :initial-value `(progn ,@body))))
