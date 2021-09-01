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
  (program-path :string))

(defun %create-gpu-context (program-path)
  (let ((context (%%create-gpu-context program-path)))
    (when (null-pointer-p context)
      (error 'gpu-context-error))
    context))

(defcfun ("an_destroy_gpu_context" %destroy-gpu-context) :void
  (ctx gpu-context))


(defcfun ("an_create_image2d" %%create-image2d) image2d
  (ctx gpu-context)
  (w   :uint)
  (h   :uint))

(defun %create-image2d (ctx w h)
  (let ((image (%%create-image2d ctx w h)))
    (when (null-pointer-p image)
      (error 'gpu-context-error))
    image))

(defcfun ("an_destroy_image2d" %destroy-image2d) :void
  (image image2d))

(defcfun ("an_image2d_get" %image2d-get) :int8
  (image image2d)
  (y     :uint)
  (x     :uint))

(defcfun ("an_image2d_set" %image2d-set) :void
  (image image2d)
  (y     :uint)
  (x     :uint)
  (val   :int8))

(defcfun ("an_image2d_fft" %%image2d-fft) :int
  (image image2d))

(defun %image2d-fft (image)
  (let ((res (%%image2d-fft image)))
    (when (zerop res)
      (error 'gpu-context-error)))
  (values))

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
