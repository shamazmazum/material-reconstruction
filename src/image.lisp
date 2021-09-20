(in-package :material-reconstruction)

(defstruct image
  array sap)

(defun create-image (context array)
  "Create a GPU image from a bit-array @c(array)"
  (declare (type gpu-context context))
  (make-image
   :array array
   :sap (%create-image2d
         (context-sap context)
         array)))

(defun destroy-image (image)
  "Destroy an image"
  (declare (type image image))
  (%destroy-image2d (image-sap image)))

(declaim (ftype
          (function (image)
                    (values (unsigned-byte 32) &optional))
          image-width image-height))

(defun image-height (image)
  (declare (type image image))
  (array-dimension (image-array image) 0))

(defun image-width (image)
  (declare (type image image))
  (array-dimension (image-array image) 1))

(declaim (ftype
          (function (image (unsigned-byte 32)
                           (unsigned-byte 32))
                    (values bit &optional))
          image-get))
(defun image-get (image x y)
  "Get image pixel at coordinates @c((x, y))."
  (declare (type image image)
           (type (unsigned-byte 32) x y))
  (aref (image-array image) y x))

(declaim (ftype
          (function (image
                     (unsigned-byte 32)
                     (unsigned-byte 32)
                     bit)
                    (values bit &optional))
          image-set))
(defun image-set (image x y val)
  "Store @c(val) to image pixel at coordinates @c((x, y))"
  (declare (type image image)
           (type (unsigned-byte 32) x y)
           (type bit val))
  (let ((delta (- val (aref (image-array image) y x))))
    (setf (aref (image-array image) y x) val)
    (%image2d-update-fft (image-sap image) y x delta))
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
