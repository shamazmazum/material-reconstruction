(in-package :material-reconstruction)

(defclass image ()
  ((array :reader        image-array
          :initarg       :array
          :type          (simple-array bit)
          :documentation "Underlying bit-array"))
  (:documentation "Basic class for two-phase images"))

(defclass image-s2 (image)
  ((sap :accessor image-sap))
  (:documentation "Class for images with associated two-point
function. @c(context) keyword argument must hold OpenCL context. The
context must remain alive while a created image lives."))

(defclass image-l2 (image)
  ((l2-void  :accessor image-l2-void
             :type     list)
   (l2-solid :accessor image-l2-solid
             :type     list))
  (:documentation "Class for images with associated lineal-path function"))

(defclass image-all (image-s2 image-l2)
  ()
  (:documentation "Class for images with lineal-path and two-point function"))

(defgeneric (setf image-pixel) (val image coord)
  (:documentation "Set image pixel at coordinates specified by
@c(coord) in row-major order to @c(val)."))

(defgeneric destroy-image (image)
  (:documentation "Destroy an image. Must be called for images which
store data on GPU (like, @c(image-s2) images).")
  (:method ((image image)) (values)))

(defun create-image (array class &rest arguments)
  "Create an image from a bit-array @c(array). Keyword argument
@c(context) is a GPU context and must be specified for images of type
@c(image-s2). @c(class) denotes a type of created image."
  (apply #'make-instance class :array array arguments))

(defmethod initialize-instance :after ((image image-s2) &key context &allow-other-keys)
  (setf (image-sap image)
        (%create-image
         (context-sap context)
         (image-array image))))

(defmethod initialize-instance :after ((image image-l2) &rest initargs)
  (declare (ignore initargs))
  (let ((array (image-array image)))
    (setf (image-l2-solid image)
          (l2 array 1)
          (image-l2-void image)
          (l2 array 0))))

(defmethod (setf image-pixel) (val (image image) coord)
  (setf (apply #'aref (image-array image) coord) val))

(defmethod (setf image-pixel) (val (image image-s2) coord)
  (declare (type bit val))
  (let ((delta (- val (image-pixel image coord))))
    (call-next-method)
    (%image-update-fft (image-sap image) delta coord))
  val)

(defun update-l2 (image coord function)
  (declare (optimize (speed 3))
           (type function function))
  (let ((l2 (list (image-l2-void  image)
                  (image-l2-solid image)))
        (array (image-array image)))
    (lua-for (phase l2-phase (enumerate l2))
      (lua-for (axis l2-dir (enumerate l2-phase))
        (declare (type (simple-array non-negative-fixnum (*)) l2-dir))
        (let ((selection (copy-list coord)))
          (setf (nth axis selection) t)
          (let ((slice (slice array selection)))
            (declare (type (simple-array bit (*)) slice))
            (map-into l2-dir function
                      l2-dir (l2-slice slice phase)))))))
  image)

(defmethod (setf image-pixel) :around (val (image image-l2) coord)
  (update-l2 image coord #'-)
  (call-next-method)
  (update-l2 image coord #'+))

(defmethod destroy-image ((image image-s2))
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

(defmacro with-image ((image array class &rest arguments) &body body)
  "Create an image @c(image) of type @c(class) and execute @c(body) in
its scope."
  `(let ((,image (create-image ,array ',class ,@arguments)))
     (unwind-protect
          (progn ,@body)
       (destroy-image ,image))))

(defmacro with-images (definitions &body body)
  "Create multiple images and execute @c(body) in the scope of those
images. Each definition in the list @c(definitions) must be in the
form @c((image-var array &rest arguments)). @c(arguments) passed
to @c(create-image) as is."
  (flet ((wrap-definition (definition forms)
           `(with-image ,definition ,forms)))
    (reduce #'wrap-definition definitions
            :from-end t
            :initial-value `(progn ,@body))))
