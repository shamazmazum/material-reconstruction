(in-package :material-reconstruction)

(define-foreign-library libanneal-ocl
  (:unix (:or #.(asdf:system-relative-pathname '#:material-reconstruction
                                               "src/libanneal-ocl.so")
              "libanneal-ocl.so"))
  (t (:default "libanneal-ocl")))

(use-foreign-library libanneal-ocl)

(defctype gpu-context :pointer)
(defctype image       :pointer)
(defctype proximeter  :pointer)

(defcfun ("an_create_gpu_context" %%create-gpu-context) gpu-context
  (program :string))

(defun %create-gpu-context (program-path)
  (let ((context (%%create-gpu-context program-path)))
    (when (null-pointer-p context)
      (error 'gpu-context-error))
    context))

(defcfun ("an_destroy_gpu_context" %destroy-gpu-context) :void
  (ctx gpu-context))


(defcfun ("an_create_image" %%create-image) image
  (ctx        gpu-context)
  (array      :pointer)
  (dimensions :pointer)
  (ndims      :uint))

(defun %create-image (ctx array)
  (declare (type (simple-array bit) array))
  (let ((buffer (map-into
                 (make-shareable-byte-vector
                  (array-total-size array))
                 #'identity
                 (aops:flatten array)))
        ;; XXX: Not "shareable array", but OK for SBCL
        (dimensions (map '(vector (unsigned-byte 32))
                         #'identity
                         (array-dimensions array))))
    (with-pointer-to-vector-data (buffer-ptr buffer)
      (with-pointer-to-vector-data (dimensions-ptr dimensions)
        (let ((image (%%create-image ctx buffer-ptr dimensions-ptr (length dimensions))))
          (when (null-pointer-p image)
            (error 'gpu-context-error))
          image)))))

(defcfun ("an_destroy_image" %destroy-image) :void
  (image image))

(defcfun ("an_image_update_fft" %%image-update-fft) :void
  (image image)
  (coord :pointer)
  (ndims :uint)
  (delta :int8))

(defun %image-update-fft (image delta coord)
  ;; XXX: coord is not "shareable array", but OK for SBCL
  (let ((coord (map '(vector (unsigned-byte 32))
                    #'identity coord)))
    (with-pointer-to-vector-data (coord-ptr coord)
      (%%image-update-fft image coord-ptr (length coord) delta))))

(defcfun ("an_create_proximeter" %%create-proximeter) proximeter
  (image1 image)
  (image2 image))

(defun %create-proximeter (image1 image2)
  (let ((proximeter (%%create-proximeter image1 image2)))
    (when (null-pointer-p proximeter)
      (error 'gpu-context-error))
    proximeter))

(defcfun ("an_destroy_proximeter" %destroy-proximeter) :void
  (proximeter proximeter))

(defcfun ("an_proximity" %proximity) :double
  (proximeter proximeter))
