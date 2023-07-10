(in-package :material-reconstruction)

(defun rfft-array-dimensions (dimensions)
  "Get dimensions of FFT of real-valued input of dimensions DIMENSIONS"
  (reduce (lambda (x acc)
            (cons
             (if (null acc)
                 (1+ (floor x 2))
                 x)
             acc))
          dimensions
          :from-end t
          :initial-value nil))

(defun list->ub32-vector (list)
  (map '(vector (unsigned-byte 32))
       #'identity list))

(defun array->ub8-vector (array)
  (map-into
   (make-shareable-byte-vector
    (array-total-size array))
   #'identity
   (aops:flatten array)))

(define-foreign-library libanneal-ocl
  (:unix (:or #.(asdf:system-relative-pathname '#:material-reconstruction
                                               "src/libanneal-ocl.so")
              "libanneal-ocl.so"))
  (t (:default "libanneal-ocl")))

(use-foreign-library libanneal-ocl)

(defctype gpu-context :pointer)
(defctype image       :pointer)

;; FFT
(defcfun ("an_rfft" %rfft) :int
  (array      :pointer)
  (real       :pointer)
  (imag       :pointer)
  (dimensions :pointer)
  (ndims      :int))

(defun rfft (array)
  "Perform FFT on an array of bits. Array may be of any sizes and dimensions."
  (declare (type (simple-array bit) array))
  (let* ((dimensions (array-dimensions array))
         (rfft-dimensions (rfft-array-dimensions dimensions))
         (real-array (make-array (reduce #'* rfft-dimensions)
                                 :element-type 'single-float))
         (imag-array (make-array (reduce #'* rfft-dimensions)
                                 :element-type 'single-float))
         (result (make-array rfft-dimensions
                             :element-type '(complex single-float)))
         (buffer (array->ub8-vector array))
         (dimensions-array (list->ub32-vector dimensions)))
    (with-pointer-to-vector-data (buffer-ptr buffer)
      (with-pointer-to-vector-data (real-ptr real-array)
        (with-pointer-to-vector-data (imag-ptr imag-array)
          (with-pointer-to-vector-data (dimensions-ptr dimensions-array)
            (when (zerop (%rfft buffer-ptr real-ptr imag-ptr dimensions-ptr
                                (length dimensions)))
              (error 'recon-error
                     :format-control "Cannot perform RFFT"))))))
    (map-into (aops:flatten result)
              (lambda (r im)
                (complex r im))
              real-array imag-array)
    result))

;; GPU context
(defcfun ("an_create_gpu_context" %%create-gpu-context) gpu-context
  (program :string))

(defun %create-gpu-context (program-path)
  (let ((context (%%create-gpu-context program-path)))
    (when (null-pointer-p context)
      (error 'recon-error
             :format-control "Cannot create GPU context"))
    context))

(defcfun ("an_destroy_gpu_context" %destroy-gpu-context) :void
  (ctx gpu-context))

;; Images
(defcfun ("an_create_image" %%create-image) image
  (ctx        gpu-context)
  (real       :pointer)
  (imag       :pointer)
  (dimensions :pointer)
  (ndims      :uint))

(defun %create-image (ctx fft-array dimensions)
  (declare (type (simple-array (complex single-float)) fft-array)
           (type list dimensions))
  (when (not (equalp (array-dimensions fft-array)
                     (rfft-array-dimensions dimensions)))
    (error 'recon-error
           :format-control "Dimensions are not compatible"))
  (let ((real (map-into
               (make-array (reduce #'* (array-dimensions fft-array))
                           :element-type 'single-float)
               #'realpart (aops:flatten fft-array)))
        (imag (map-into
               (make-array (reduce #'* (array-dimensions fft-array))
                           :element-type 'single-float)
               #'imagpart (aops:flatten fft-array)))
        ;; XXX: Not "shareable array", but OK for SBCL
        (dimensions (list->ub32-vector dimensions)))
    (with-pointer-to-vector-data (real-ptr real)
      (with-pointer-to-vector-data (imag-ptr imag)
      (with-pointer-to-vector-data (dimensions-ptr dimensions)
        (let ((image (%%create-image ctx real-ptr imag-ptr dimensions-ptr
                                     (length dimensions))))
          (when (null-pointer-p image)
            (error 'recon-error
                   :format-control "Cannot upload image to GPU"))
          image))))))

(defcfun ("an_create_corrfn" %%create-corrfn) image
  (ctx        gpu-context)
  (corrfn     :pointer)
  (dimensions :pointer)
  (ndims      :uint))

(defun %create-corrfn (ctx corrfn-array dimensions)
  (declare (type (simple-array single-float) corrfn-array)
           (type list dimensions))
  (when (not (equalp (array-dimensions corrfn-array)
                     (rfft-array-dimensions dimensions)))
    (error 'recon-error
           :format-control "Dimensions are not compatible"))
  (let ((corrfn-array (map-into
                       (make-array (reduce #'* (array-dimensions corrfn-array))
                                   :element-type 'single-float)
                       #'identity (aops:flatten corrfn-array)))
        (dimensions (list->ub32-vector dimensions)))
    (with-pointer-to-vector-data (corrfn-ptr corrfn-array)
      (with-pointer-to-vector-data (dimensions-ptr dimensions)
        (let ((image (%%create-corrfn ctx corrfn-ptr dimensions-ptr
                                      (length dimensions))))
          (when (null-pointer-p image)
            (error 'recon-error
                   :format-control "Cannot upload image to GPU"))
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

(defcfun ("an_distance" %%distance) :int
  (target   image)
  (recon    image)
  (distance :pointer))

(defun %distance (target-sap recon-sap)
  (with-foreign-object (distance-ptr :float)
    (when (zerop (%%distance target-sap recon-sap distance-ptr))
      (error 'recon-error
             :format-control "Cannot calculate distance between images"))
    (mem-ref distance-ptr :float)))
