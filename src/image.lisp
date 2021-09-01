(in-package :material-reconstruction)

(defstruct image
  sap
  (width  0 :type (unsigned-byte 32))
  (height 0 :type (unsigned-byte 32)))

(defun create-image (context w h)
  (declare (type gpu-context context)
           (type (unsigned-byte 32) w h))
  (make-image
   :sap (%create-image2d
         (context-sap context)
         w h)
   :width w
   :height h))

(defun destroy-image (image)
  (declare (type image image))
  (%destroy-image2d (image-sap image)))

(defun load-image (image array)
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
  (%image2d-fft (image-sap image))
  image)

(declaim (ftype
          (function (image (unsigned-byte 32)
                           (unsigned-byte 32))
                    (values (signed-byte 8) &optional))
          image-get))
(defun image-get (image x y)
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
  `(let ((,image (create-image ,context ,width ,height)))
     (unwind-protect
          (progn ,@body)
       (destroy-image ,image))))

(defmacro with-images (definitions &body body)
  (flet ((wrap-definition (definition forms)
           `(with-image ,definition ,forms)))
    (reduce #'wrap-definition definitions
            :from-end t
            :initial-value `(progn ,@body))))

(defun image->array (image)
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
