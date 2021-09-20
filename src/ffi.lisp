(in-package :material-reconstruction)

(define-foreign-library libanneal-ocl
  (:unix (:or #.(asdf:system-relative-pathname '#:material-reconstruction
                                               "src/libanneal-ocl.so")
              "libanneal-ocl.so"))
  (t (:default "libanneal-ocl")))

(use-foreign-library libanneal-ocl)

(defctype gpu-context :pointer)
(defctype image2d     :pointer)
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


(defcfun ("an_create_image2d" %%create-image2d) image2d
  (ctx   gpu-context)
  (array :pointer)
  (w     :uint)
  (h     :uint))

(defun %create-image2d (ctx array)
  (declare (type (simple-array bit (* *)) array))
  (let ((height (array-dimension array 0))
        (width  (array-dimension array 1))
        (buffer (map-into
                 (make-shareable-byte-vector
                  (array-total-size array))
                 #'identity
                 (aops:flatten array))))
    (with-pointer-to-vector-data (buffer-ptr buffer)
      (let ((image (%%create-image2d ctx buffer-ptr width height)))
        (when (null-pointer-p image)
          (error 'gpu-context-error))
        image))))

(defcfun ("an_destroy_image2d" %destroy-image2d) :void
  (image image2d))

(defcfun ("an_image2d_update_fft" %image2d-update-fft) :void
  (image image2d)
  (y     :uint)
  (x     :uint)
  (delta :int8))

(defcfun ("an_create_proximeter" %%create-proximeter) proximeter
  (image1 image2d)
  (image2 image2d))

(defun %create-proximeter (image1 image2)
  (let ((proximeter (%%create-proximeter image1 image2)))
    (when (null-pointer-p proximeter)
      (error 'gpu-context-error))
    proximeter))

(defcfun ("an_destroy_proximeter" %destroy-proximeter) :void
  (proximeter proximeter))

(defcfun ("an_proximity" %proximity) :double
  (proximeter proximeter))
