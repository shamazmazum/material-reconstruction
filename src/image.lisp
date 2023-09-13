(in-package :material-reconstruction)

(sera:defconstructor image-change
  (coord list)
  (value bit))

(defclass image ()
  ((array      :reader        image-array
               :initarg       :array
               :type          (simple-array bit)
               :documentation "Underlying bit-array")
   (changelist :accessor      image-changelist
               :initform      nil
               :type          list
               :documentation "List of changes since the previous accepted modification"))
  (:documentation "Basic class for two-phase images"))

(defclass image-s2 (image gpu-object)
  ((periodic-p :initarg  :periodic-p
               :initform t
               :type     boolean
               :accessor image-s2-periodic-p
               :documentation "If autocorrelation is periodic or not"))
  (:documentation "Class for images with associated two-point
function. @c(context) keyword argument must hold OpenCL context. The
context must remain alive while a created image lives."))

(defgeneric (setf image-pixel) (val image coord)
  (:documentation "Set image pixel at coordinates specified by
@c(coord) in row-major order to @c(val)."))

(defgeneric image-start-modification (image)
  (:documentation "Start a new change, invalidating any previous
rollback information."))

(defgeneric image-rollback (image)
  (:documentation "Rollback the image to the state of the previous
call to @c(image-start-modification)"))

(defmethod initialize-instance :after ((image image-s2) &key context &allow-other-keys)
  (let ((array (funcall
                (if (image-s2-periodic-p image)
                    #'identity #'pad-with-zeros)
                (image-array image))))
    (setf (object-sap image)
          (%create-image
           (object-sap context)
           (rfft array)
           (array-dimensions array)))))

(defmethod image-start-modification ((image image))
  (setf (image-changelist image) nil))

(defmethod image-start-modification :after ((image image-s2))
  (%image-store-state (object-sap image)))

(defmethod image-rollback ((image image))
  (loop for change in (image-changelist image) do
        (setf (apply #'aref (image-array image)
                     (image-change-coord change))
              (image-change-value change)))
  (setf (image-changelist image) nil))

(defmethod image-rollback :after ((image image-s2))
  (%image-rollback (object-sap image)))

(defmethod (setf image-pixel) (val (image image) coord)
  (symbol-macrolet ((pixel-in-image (apply #'aref (image-array image) coord)))
    (push (image-change coord pixel-in-image)
          (image-changelist image))
    (setf pixel-in-image val)))

(defmethod (setf image-pixel) (val (image image-s2) coord)
  (declare (type bit val))
  (let ((delta (- val (image-pixel image coord))))
    (call-next-method)
    (%image-update-fft (object-sap image) delta coord))
  val)

(defmethod destroy-gpu-object ((image-s2 image-s2))
  (%destroy-image (object-sap image-s2)))

(sera:-> image-dimensions (image) (values list &optional))
(defun image-dimensions (image)
  "Get image dimensions."
  (array-dimensions (image-array image)))

(sera:-> image-total-size (image) (values alex:positive-fixnum &optional))
(defun image-total-size (image)
  "Get total number of elements in the image."
  (array-total-size (image-array image)))

(defun coordinates-p (list)
  (every (lambda (x) (typep x '(unsigned-byte 32))) list))

(sera:-> image-pixel (image (satisfies coordinates-p)) (values bit &optional))
(defun image-pixel (image coord)
  "Get image pixel at coordinates specified by @c(coord) in row-major
order. Also can serve as a place for @c(setf)."
  (apply #'aref (image-array image) coord))

(sera:-> image-gpu-s2 (image)
         (values (simple-array fixnum) &optional))
(defun image-gpu-s2 (image)
  (let ((dimensions (funcall
                     (if (image-s2-periodic-p image)
                         #'identity #'dimensions-with-padding)
                     (image-dimensions image))))
    (s2-from-dft
     (%image-get (object-sap image) dimensions)
     dimensions)))

(deftype update-type () '(member :pre :post))
(defclass update-callback-mixin ()
  ((update-callback :reader   update-callback
                    :type     function
                    :initarg  :callback
                    :initform (error "Specify the callback")
                    :documentation "A function which is called with
three arguments: the first argument is an image object the second is
an index at which update is performed and the third is the update type
(@c(:pre) or @c(:post))."))
  (:documentation "A mixin with a callback which will be called when a
pixel is changed in the image. It's called twice: the first time
before the actual change is made and the second time after the change
is made. For example, image classes whose instances are used with
@c(dpn-sampler) must be subclasses of @c(update-callback-mixin)."))

(defmethod (setf image-pixel) :around (val (object update-callback-mixin) coord)
  (let ((updatedp (/= val (image-pixel object coord))))
    (when updatedp
      (funcall (update-callback object) object coord :pre))
    (call-next-method)
    (when updatedp
      (funcall (update-callback object) object coord :post))))
