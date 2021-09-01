(in-package :material-reconstruction)

(defstruct proximeter
  sap image1 image2)

(defun create-proximeter (image1 image2)
  (declare (type image image1 image2))
  (make-proximeter
   :image1 image1
   :image2 image2
   :sap (%create-proximeter (image-sap image1)
                            (image-sap image2))))

(defun destroy-proximeter (proximeter)
  (%destroy-proximeter (proximeter-sap proximeter)))

(declaim (ftype
          (function (proximeter)
                    (values double-float &optional))
          proximity))
(defun proximity (proximeter)
  (%proximity (proximeter-sap proximeter)))

(defmacro with-proximeter ((proximeter image1 image2) &body body)
  `(let ((,proximeter (create-proximeter ,image1 ,image2)))
     (unwind-protect
          (progn ,@body)
       (destroy-proximeter ,proximeter))))
