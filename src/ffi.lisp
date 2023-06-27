(in-package :material-reconstruction)

(define-foreign-library libanneal
  (:unix (:or #.(asdf:system-relative-pathname '#:material-reconstruction
                                               "src/libanneal.so")
              "libanneal.so"))
  (t (:default "libanneal")))

(use-foreign-library libanneal)

(defctype gpu-context :pointer)
(defctype image       :pointer)

(defun list->ub32-vector (list)
  (map '(vector (unsigned-byte 32))
       #'identity list))

(defun dimensions-rdft (dimensions)
  (loop with length = (length dimensions)
        for d in dimensions
        for i from 0 by 1 collect
        (if (= i (1- length))
            (1+ (floor d 2)) d)))

;; DFT calculation
(defcfun ("an_rfft" %rfft) :int
  (in         (:pointer :float))
  (out        (:pointer :float))
  (dimensions (:pointer :uint32))
  (ndims      :uint32))

(-> rfft ((simple-array single-float))
    (values (simple-array (complex single-float)) &optional))
(defun rfft (array)
  "Calculate DFT for an array of single floats"
  (let* ((dimensions (array-dimensions array))
         (dft-dimensions (dimensions-rdft dimensions))
         (input-buffer   (map-into
                          (make-array (array-total-size array)
                                      :element-type 'single-float)
                          #'identity
                          (aops:flatten array)))
         (output (make-array dft-dimensions
                             :element-type '(complex single-float)))
         (output-buffer (make-array (* 2 (reduce #'* dft-dimensions))
                                    :element-type 'single-float))
         (dim-vector (list->ub32-vector dimensions)))
    (with-pointer-to-vector-data (input-ptr input-buffer)
      (with-pointer-to-vector-data (output-ptr output-buffer)
          (with-pointer-to-vector-data (dim-ptr dim-vector)
            (when (zerop (%rfft input-ptr output-ptr dim-ptr (array-rank array)))
              (error 'recon-error
                     :format-control "Cannot calculate DFT function")))))
    (loop for i below (array-total-size output) do
          (setf (row-major-aref output i)
                (complex
                 (aref output-buffer (* i 2))
                 (aref output-buffer (1+ (* i 2))))))
    output))

(defcfun ("an_irfft" %irfft) :int
  (in         (:pointer :float))
  (out        (:pointer :float))
  (dimensions (:pointer :uint32))
  (ndims      :uint32))

(-> irfft ((simple-array (complex single-float)) list)
    (values (simple-array single-float) &optional))
(defun irfft (array dimensions)
  (unless (equalp (array-dimensions array)
                  (dimensions-rdft dimensions))
    (error 'recon-error
           :format-control "DFT: Dimensions mismatch (~a vs ~a)"
           :format-arguments (list (array-dimensions array) dimensions)))
  (let ((input-buffer (make-array (* 2 (array-total-size array))
                                  :element-type 'single-float))
        (output-buffer (make-array (reduce #'* dimensions)
                                   :element-type 'single-float))
        (output (make-array dimensions :element-type 'single-float))
        (dim-vector (list->ub32-vector dimensions)))
    (loop for i below (array-total-size array) do
          (let ((element (row-major-aref array i)))
            (setf (aref input-buffer (* i 2))
                  (realpart element)
                  (aref input-buffer (1+ (* i 2)))
                  (imagpart element))))
    (with-pointer-to-vector-data (input-ptr input-buffer)
      (with-pointer-to-vector-data (output-ptr output-buffer)
        (with-pointer-to-vector-data (dim-ptr dim-vector)
            (when (zerop (%irfft input-ptr output-ptr dim-ptr (array-rank array)))
              (error 'recon-error
                     :format-control "Cannot calculate DFT function")))))
    (map-into (aops:flatten output) #'identity output-buffer)
    output))

;; GPU context
(defcfun ("an_create_gpu_context" %%create-gpu-context) gpu-context
  (program :string))

(defun %create-gpu-context (program)
  (let ((context (%%create-gpu-context program)))
    (when (null-pointer-p context)
      (error 'recon-error :format-control "Cannot create GPU context"))
    context))

(defcfun ("an_destroy_gpu_context" %destroy-gpu-context) :void
  (ctx gpu-context))


;; Images
(defcfun ("an_create_image" %%create-image) image
  (ctx        gpu-context)
  (image      (:pointer :uint8))
  (s2         (:pointer :uint64))
  (dimensions (:pointer :uint32))
  (s2-shifts  (:pointer :uint32))
  (ndims      :uint))

(defun %create-image (ctx image shifts)
  (let ((s2-buffer
         (let ((s2 (s2 image shifts)))
           (map-into
            (make-array (array-total-size s2)
                        :element-type '(unsigned-byte 64))
            #'identity (aops:flatten s2))))
        (image-buffer (map-into
                       (make-array (array-total-size image)
                                   :element-type '(unsigned-byte 8))
                       #'identity (aops:flatten image)))
        (dimensions (map '(vector (unsigned-byte 32))
                         #'identity (array-dimensions image)))
        (shifts (map '(vector (unsigned-byte 32))
                     #'identity shifts)))
    (with-pointer-to-vector-data (image-ptr image-buffer)
      (with-pointer-to-vector-data (s2-ptr s2-buffer)
        (with-pointer-to-vector-data (dimensions-ptr dimensions)
          (with-pointer-to-vector-data (shifts-ptr shifts)
          (let ((gpu-image (%%create-image ctx image-ptr s2-ptr dimensions-ptr shifts-ptr
                                           (array-rank image))))
            (when (null-pointer-p gpu-image)
              (error 'recon-error :format-control "Cannot create GPU image"))
            gpu-image)))))))

;; FIXME: Can we infer SHIFTS from S2?
(defun %create-corrfn (ctx s2 shifts)
  (let ((s2-buffer (map-into
                    (make-array (array-total-size s2)
                                :element-type '(unsigned-byte 64))
                    #'identity (aops:flatten s2)))
        (shifts (map '(vector (unsigned-byte 32))
                     #'identity shifts)))
    (with-pointer-to-vector-data (s2-ptr s2-buffer)
      (with-pointer-to-vector-data (shifts-ptr shifts)
        (let ((gpu-image (%%create-image ctx (cffi:null-pointer) s2-ptr (cffi:null-pointer) shifts-ptr
                                         (array-rank s2))))
          (when (null-pointer-p gpu-image)
            (error 'recon-error :format-control "Cannot create S₂ GPU image"))
          gpu-image)))))

(defcfun ("an_destroy_image" %destroy-image) :void
  (image image))

;; S₂ updates
(defcfun ("an_image_flip_pixel" %%image-flip-pixel) :int
  (image image)
  (coord (:pointer :uint32)))

(defun %image-flip-pixel (image-sap coord)
  (let ((coord-buffer (make-array (length coord)
                                  :element-type '(unsigned-byte 32)
                                  :initial-contents coord)))
    (with-pointer-to-vector-data (coord-ptr coord-buffer)
      (let ((res (%%image-flip-pixel image-sap coord-ptr)))
        (when (zerop res)
          (error 'recon-error :format-control "Cannot flip pixel on GPU")))))
  coord)

;; S₂ metric
(defcfun ("an_distance" %%distance) :int
  (target   image)
  (recon    image)
  (distance (:pointer :uint64)))

(defun %distance (target-sap recon-sap)
  (with-foreign-object (distance-ptr :uint64)
    (when (zerop (%%distance target-sap recon-sap distance-ptr))
      (error 'recon-error
             :format-control "Cannot calculate distance between images"))
    (mem-ref distance-ptr :uint64)))

;; Debugging & tests

;; FIXME: Works only for even dimensions of S₂
(defun shifts->s2-dimensions (shifts)
  (loop for s in shifts
        for i from 1 by 1 collect
        (if (= i (length shifts))
            (1+ s) (* 2 s))))

(defcfun ("an_image_get_s2" %%image-s2) :int
  (image image)
  (s2    (:pointer :uint64)))

(defun %image-s2 (image shifts)
  (let* ((s2-dimensions (shifts->s2-dimensions shifts))
         (s2 (make-array s2-dimensions :element-type '(unsigned-byte 64)))
         (s2-buffer (make-array (reduce #'* s2-dimensions) :element-type '(unsigned-byte 64))))
    (with-pointer-to-vector-data (s2-ptr s2-buffer)
      (let ((res (%%image-s2 image s2-ptr)))
        (when (zerop res)
          (error 'recon-error :format-control "Cannot read S₂ from GPU"))))
    (map-into (aops:flatten s2) #'identity s2-buffer)
    s2))
