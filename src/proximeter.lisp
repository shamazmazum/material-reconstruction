(in-package :material-reconstruction)

(defstruct proximeter
  "Proximeter is a handle which must be used to get a distance between
two images according to two-point correlation functon."
  sap image1 image2)

(defun create-proximeter (image1 image2)
  "Create a @c(proximeter) structure which measures distance between
@c(image1) and @c(image2)."
  (declare (type image image1 image2))
  (make-proximeter
   :image1 image1
   :image2 image2
   :sap (%create-proximeter (image-sap image1)
                            (image-sap image2))))

(defun destroy-proximeter (proximeter)
  "Destroy proximeter."
  (%destroy-proximeter (proximeter-sap proximeter)))

(declaim (ftype
          (function (proximeter)
                    (values double-float &optional))
          proximity))
(defun proximity (proximeter)
  "Return a distance (a double-precision floating point value) between
tracked images according to two-point correlation function."
  (%proximity (proximeter-sap proximeter)))

(defmacro with-proximeter ((proximeter image1 image2) &body body)
  "Create proximeter and execute @c(body) in its scope."
  `(let ((,proximeter (create-proximeter ,image1 ,image2)))
     (unwind-protect
          (progn ,@body)
       (destroy-proximeter ,proximeter))))
