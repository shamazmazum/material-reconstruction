(in-package :material-reconstruction)

(defstruct image
  sap
  (width  0 :type (unsigned-byte 32))
  (height 0 :type (unsigned-byte 32)))

(defun create-image (context w h)
  "Create an image with width @c(w) and height @c(h). Valid OpenCL
context @c(context) must be specified."
  (declare (type gpu-context context)
           (type (unsigned-byte 32) w h))
  (make-image
   :sap (%create-image2d
         (context-sap context)
         w h)
   :width w
   :height h))

(defun destroy-image (image)
  "Destroy an image"
  (declare (type image image))
  (%destroy-image2d (image-sap image)))

(defun load-image (image array)
  "Load two-dimensional array @c(array) into image @c(image)."
  (declare (type image image)
           (type (simple-array (signed-byte 8)) array))
  (when (or (/= (image-width image)
                (array-dimension array 1))
            (/= (image-height image)
                (array-dimension array 0)))
    (error 'recon-simple-error
           :format-control "Image and array have different dimensions"))

  (nested-loop (i j)
      (array-dimensions array)
    (%image2d-set
     (image-sap image)
     i j
     (aref array i j)))
  (image-fft image))

(declaim (ftype
          (function (image (unsigned-byte 32)
                           (unsigned-byte 32))
                    (values (signed-byte 8) &optional))
          image-get))
(defun image-get (image x y)
  "Get image pixel at coordinates @c((x, y))."
  (declare (type image image)
           (type (unsigned-byte 32) x y))
  (when (or (>= x (image-width image))
            (>= y (image-height image)))
    (error 'recon-simple-error
           :format-control "(~d, ~d): index too big"
           :format-arguments (list x y)))
  (%image2d-get (image-sap image) x y))

(declaim (ftype
          (function (image (unsigned-byte 32)
                           (unsigned-byte 32)
                           (signed-byte 8))
                    (values &optional))
          image-set))
(defun image-set (image x y val)
  "Store @c(val) to image pixel at coordinates @c((x, y))"
  (declare (type image image)
           (type (unsigned-byte 32) x y)
           (type (signed-byte 8) val))
  (when (or (>= x (image-width image))
            (>= y (image-height image)))
    (error 'recon-simple-error
           :format-control "(~d, ~d): index too big"
           :format-arguments (list x y)))
  (%image2d-set (image-sap image) x y val))

(defmacro with-image ((image context width height) &body body)
  "Create an image @c(image) and execute @c(body) in its scope."
  `(let ((,image (create-image ,context ,width ,height)))
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

(defun image->array (image)
  "Copy contents of an image to an array."
  (let ((array (make-array (list (image-height image)
                                 (image-width  image))
                           :element-type '(signed-byte 8))))
    (nested-loop (i j)
        (array-dimensions array)
      (setf (aref array i j)
            (image-get image i j)))
    array))

(defun image-fft (image)
  (%image2d-fft (image-sap image))
  image)
