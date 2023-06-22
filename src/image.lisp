(in-package :material-reconstruction)

(defclass image ()
  ((array :reader        image-array
          :initarg       :array
          :type          (simple-array bit)
          :documentation "Underlying bit-array"))
  (:documentation "Basic class for two-phase images"))

(defclass image-s2 (image gpu-object)
  ()
  (:documentation "Class for images with associated two-point
function. @c(context) keyword argument must hold OpenCL context. The
context must remain alive while a created image lives."))

(defclass image-l2 (image)
  ((l2 :accessor      image-l2
       :type          corrfn-l2
       :documentation "Lineal-path function of the image"))
  (:documentation "Class for images with associated lineal-path function"))

(defgeneric (setf image-pixel) (val image coord)
  (:documentation "Set image pixel at coordinates specified by
@c(coord) in row-major order to @c(val)."))

(defgeneric image-gpu-s2 (image)
  (:documentation "Get S₂ function from GPU memory"))

(defmethod initialize-instance :after ((image image-s2) &key context &allow-other-keys)
  (setf (object-sap image)
        (%create-image
         (object-sap context)
         (image-array image))))

(defmethod initialize-instance :after ((image image-l2) &rest initargs)
  (declare (ignore initargs))
  (setf (image-l2 image)
        (make-instance 'corrfn-l2
                       :array (image-array image))))

(defmethod (setf image-pixel) (val (image image) coord)
  (setf (apply #'aref (image-array image) coord) val))

(defmethod (setf image-pixel) (val (image image-s2) coord)
  (when (/= (image-pixel image coord) val)
    (%image-flip-pixel (object-sap image) coord))
  (call-next-method))

(defun update-l2 (image coord function)
  (declare (optimize (speed 3))
           (type function function))
  (let ((l2 (list (l2-void  (image-l2 image))
                  (l2-solid (image-l2 image))))
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

(defmethod image-gpu-s2 ((image-s2 image-s2))
  (%image-s2 (object-sap image-s2)
             (array-dimensions (image-array image-s2))))

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
