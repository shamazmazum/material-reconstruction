(in-package :material-reconstruction)

(defstruct image
  array sap)

(defun create-image (context array)
  "Create a GPU image from a bit-array @c(array)"
  (declare (type gpu-context context))
  (make-image
   :array array
   :sap (%create-image
         (context-sap context)
         array)))

(defun destroy-image (image)
  "Destroy an image"
  (declare (type image image))
  (%destroy-image (image-sap image)))

(declaim
 (ftype
  (function (image) (values list &optional))
  image-dimensions))

(defun image-dimensions (image)
  (declare (type image image))
  (array-dimensions (image-array image)))

(declaim (ftype
          (function (image (cons (unsigned-byte 32)))
                    (values bit &optional))
          image-pixel))
(defun image-pixel (image coord)
  "Get image pixel at coordinates specified by @c(coord) in row-major
order."
  (declare (type image image))
  (apply #'aref (image-array image) coord))

(declaim (ftype
          (function (bit image (cons (unsigned-byte 32)))
                    (values bit &optional))
          (setf image-pixel)))
(defun (setf image-pixel) (val image coord)
  "Set image pixel at coordinates specified by @c(coord) in row-major
order to @c(val)."
  (declare (type image image)
           (type bit val))
  (let ((delta (- val (image-pixel image coord))))
    (setf (apply #'aref (image-array image) coord) val)
    (%image-update-fft (image-sap image) delta coord))
  val)

(defmacro with-image ((image context array) &body body)
  "Create an image @c(image) and execute @c(body) in its scope."
  `(let ((,image (create-image ,context ,array)))
     (unwind-protect
          (progn ,@body)
       (destroy-image ,image))))

(defmacro with-images (definitions &body body)
  "Create multiple images and execute @c(body) in the scope of those
images. Each definition in the list @c(definitions) must be in the
form @c((image-var context width height))."
  (flet ((wrap-definition (definition forms)
           `(with-image ,definition ,forms)))
    (reduce #'wrap-definition definitions
            :from-end t
            :initial-value `(progn ,@body))))
