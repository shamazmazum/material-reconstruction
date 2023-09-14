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

(unless (uiop:getenv "CI")
  (define-foreign-library libannealing-lowlevel
    (:unix (:or "libannealing-lowlevel.so"))
    (t (:default "libannealing-lowlevel")))
  (use-foreign-library libannealing-lowlevel))

(defctype gpu-context :pointer)
(defctype image       :pointer)
(defctype corrfn      :pointer)
(defctype metric      :pointer)

;; FFT
(defcfun ("an_rfft" %rfft) :int
  (array      (:pointer :float))
  (real       (:pointer :float))
  (imag       (:pointer :float))
  (dimensions (:pointer :uint32))
  (ndims      :int))

(defun rfft (array)
  "Perform RFFT on an array. Array may be of any sizes and dimensions."
  (let* ((dimensions (array-dimensions array))
         (rfft-dimensions (rfft-array-dimensions dimensions))
         (real-array (make-array (reduce #'* rfft-dimensions)
                                 :element-type 'single-float))
         (imag-array (make-array (reduce #'* rfft-dimensions)
                                 :element-type 'single-float))
         (result (make-array rfft-dimensions
                             :element-type '(complex single-float)))
         (buffer (map '(vector single-float) #'float (aops:flatten array)))
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

(defcfun ("an_irfft" %irfft) :int
  (array      (:pointer :float))
  (real       (:pointer :float))
  (imag       (:pointer :float))
  (dimensions (:pointer :uint32))
  (ndims      :int))

(defun irfft (array dimensions)
  "Perform IRFFT on an array of complex single-precision values."
  (unless (every #'=
                 (array-dimensions array)
                 (rfft-array-dimensions dimensions))
    (error 'recon-error
           :format-control "Array dimensions of the FFT and IFFT do not match: ~a vs ~a"
           :format-arguments (list (array-dimensions array) dimensions)))
  (let ((real-buffer   (make-array (array-total-size array) :element-type 'single-float))
        (imag-buffer   (make-array (array-total-size array) :element-type 'single-float))
        (result-buffer (make-array (reduce #'* dimensions)  :element-type 'single-float))
        (result        (make-array dimensions :element-type 'single-float))
        (dimensions-array (list->ub32-vector dimensions)))
    ;; Fill input
    (map-into real-buffer #'realpart (aops:flatten array))
    (map-into imag-buffer #'imagpart (aops:flatten array))
    (with-pointer-to-vector-data (real-ptr real-buffer)
      (with-pointer-to-vector-data (imag-ptr imag-buffer)
        (with-pointer-to-vector-data (result-ptr result-buffer)
          (with-pointer-to-vector-data (dimensions-ptr dimensions-array)
            (when (zerop (%irfft result-ptr real-ptr imag-ptr dimensions-ptr
                                (length dimensions)))
              (error 'recon-error
                     :format-control "Cannot perform RFFT"))))))
    (map-into (aops:flatten result) #'identity result-buffer)
    result))

;; GPU context
(defcfun ("an_create_context" %%create-gpu-context) gpu-context
  (ndim       :uint)
  (validation (:boolean :int)))

(defun %create-gpu-context (ndim validation)
  (let ((context (%%create-gpu-context ndim validation)))
    (when (null-pointer-p context)
      (error 'recon-error
             :format-control "Cannot create GPU context"))
    context))

(defcfun ("an_destroy_context" %destroy-gpu-context) :void
  (ctx gpu-context))

;; Images
(defcfun ("an_create_image" %%create-image) image
  (ctx        gpu-context)
  (real       (:pointer :float))
  (imag       (:pointer :float))
  (dimensions (:pointer :uint32))
  (ndim       :uint))

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

(defcfun ("an_create_corrfn" %%create-corrfn) corrfn
  (ctx        gpu-context)
  (corrfn     (:pointer :float))
  (dimensions (:pointer :uint32))
  (ndim       :uint))

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

(defcfun ("an_destroy_corrfn" %destroy-corrfn) :void
  (corrfn corrfn))

(defcfun ("an_image_update_fft" %%image-update-fft) :void
  (image image)
  (coord (:pointer :uint32))
  (ndim  :uint)
  (delta :float))

(defun %image-update-fft (image delta coord)
  ;; XXX: coord is not "shareable array", but OK for SBCL
  (let ((coord (map '(vector (unsigned-byte 32))
                    #'identity coord)))
    (with-pointer-to-vector-data (coord-ptr coord)
      (%%image-update-fft image coord-ptr
                          (length coord)
                          (float delta)))))

(defcfun ("an_image_get" %%image-get) :int
  (image image)
  (real (:pointer :float))
  (imag (:pointer :float)))

(defun %image-get (image dimensions)
  (let* ((dft-dimensions (rfft-array-dimensions dimensions))
         (total-size (reduce #'* dft-dimensions))
         (real-buffer (make-array total-size :element-type 'single-float))
         (imag-buffer (make-array total-size :element-type 'single-float))
         (result (make-array dft-dimensions :element-type '(complex single-float))))
    (with-pointer-to-vector-data (real-ptr real-buffer)
      (with-pointer-to-vector-data (imag-ptr imag-buffer)
        (when (zerop (%%image-get image real-ptr imag-ptr))
          (error 'recon-error
                 :format-control "Cannot upload image from GPU"))))
    (map-into
     (aops:flatten result)
     #'complex real-buffer imag-buffer)
    result))

;; Metric
(defcfun ("an_create_metric" %create-metric) metric
  (ctx        gpu-context)
  (target     corrfn)
  (recon      image))

(defcfun ("an_destroy_metric" %destroy-metric) :void
  (metric metric))

(defcfun ("an_distance" %%distance) :int
  (metric metric)
  (distance (:pointer :float)))

(defun %distance (metric-sap)
  (with-foreign-object (distance-ptr :float)
    (when (zerop (%%distance metric-sap distance-ptr))
      (error 'recon-error
             :format-control "Cannot calculate distance between images"))
    (mem-ref distance-ptr :float)))
