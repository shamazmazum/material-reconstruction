(in-package :material-reconstruction)

(defclass image ()
  ((array :reader        image-array
          :initarg       :array
          :type          (simple-array bit)
          :documentation "Underlying bit-array"))
  (:documentation "Basic class for two-phase images"))

(defclass image-s2 (image gpu-object)
  ((shifts :initarg  :shifts
           :accessor image-s2-shifts
           :initform nil
           :type     list))
  (:documentation "Class for images with associated two-point
function. @c(context) keyword argument must hold OpenCL context. The
context must remain alive while a created image lives."))

(defgeneric (setf image-pixel) (val image coord)
  (:documentation "Set image pixel at coordinates specified by
@c(coord) in row-major order to @c(val)."))

(defgeneric image-gpu-s2 (image)
  (:documentation "Get S₂ function from GPU memory"))

(defmethod initialize-instance :after ((image image-s2)
                                       &key context &allow-other-keys)
  (let ((array (image-array image)))
    (unless (image-s2-shifts image)
      (setf (image-s2-shifts image)
            (maximal-shifts array)))
    (setf (object-sap image)
          (%create-image
           (object-sap context) array
           (image-s2-shifts image)))))

(defmethod (setf image-pixel) (val (image image) coord)
  (setf (apply #'aref (image-array image) coord) val))

(defmethod (setf image-pixel) (val (image image-s2) coord)
  (when (/= (image-pixel image coord) val)
    (%image-flip-pixel (object-sap image) coord))
  (call-next-method))

(defmethod image-gpu-s2 ((image-s2 image-s2))
  (%image-s2 (object-sap image-s2)
             (image-s2-shifts image-s2)))

(defmethod destroy-gpu-object ((image-s2 image-s2))
  (%destroy-image (object-sap image-s2)))

(-> image-dimensions (image) (values list &optional))
(defun image-dimensions (image)
  "Get image dimensions."
  (declare (type image image))
  (array-dimensions (image-array image)))

(defun coordinates-p (list)
  (every (lambda (x) (typep x '(unsigned-byte 32))) list))

(-> image-pixel (image (satisfies coordinates-p)) (values bit &optional))
(defun image-pixel (image coord)
  "Get image pixel at coordinates specified by @c(coord) in row-major
order. Also can serve as a place for @c(setf)."
  (declare (type image image))
  (apply #'aref (image-array image) coord))
