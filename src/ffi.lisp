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

(defun array->ub8-vector (array)
  (map-into
   (make-shareable-byte-vector
    (array-total-size array))
   #'identity
   (aops:flatten array)))

(defun dimensions-image->s2 (dimensions)
  (loop with length = (length dimensions)
        for d in dimensions
        for i from 0 by 1 collect
        (if (= i (1- length))
            (1+ (floor d 2)) d)))

(defun dimensions->ranges (dimensions)
  (mapcar
   (lambda (d)
     (select:range 0 d))
   dimensions))

;; S₂ calculation
(defcfun ("an_s2" %s2) :int
  (array      (:pointer :uint8))
  (s2         (:pointer :uint64))
  (dimensions (:pointer :uint32))
  (ndims      :uint32))

(-> s2 ((simple-array bit))
         (values (simple-array (unsigned-byte 64)) &optional))
(defun s2 (array)
  "Calculate S₂ function for multidimensional bit array ARRAY."
  (let ((input (array->ub8-vector array))
        (output (make-array (array-total-size array) :element-type 'single-float))
        (s2 (make-array (array-dimensions array) :element-type '(unsigned-byte 64)))
        (dim-array (list->ub32-vector (array-dimensions array))))
    (with-pointer-to-vector-data (input-ptr input)
      (with-pointer-to-vector-data (output-ptr output)
          (with-pointer-to-vector-data (dim-ptr dim-array)
            (when (zerop (%s2 input-ptr output-ptr dim-ptr (array-rank array)))
              (error 'recon-error
                     :format-control "Cannot calculate S₂ function")))))
    (map-into (aops:flatten s2)
              (lambda (x)
                (round (/ x (array-total-size array))))
               output)
    (apply #'select:select s2
           (dimensions->ranges
            (dimensions-image->s2
             (array-dimensions array))))))

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
  (ndims      :uint))

(defun %create-image (ctx image)
  (let ((s2-buffer
         (let ((s2 (s2 image)))
           (map-into
            (make-array (array-total-size s2)
                        :element-type '(unsigned-byte 64))
            #'identity (aops:flatten s2))))
        (image-buffer (map-into
                       (make-array (array-total-size image)
                                   :element-type '(unsigned-byte 8))
                       #'identity (aops:flatten image)))
        (dimensions (map '(vector (unsigned-byte 32))
                         #'identity (array-dimensions image))))
    (with-pointer-to-vector-data (image-ptr image-buffer)
      (with-pointer-to-vector-data (s2-ptr s2-buffer)
        (with-pointer-to-vector-data (dimensions-ptr dimensions)
          (let ((gpu-image (%%create-image ctx image-ptr s2-ptr dimensions-ptr (array-rank image))))
            (when (null-pointer-p gpu-image)
              (error 'recon-error :format-control "Cannot create GPU image"))
            gpu-image))))))

(defun %create-corrfn (ctx s2 dimensions)
  (let ((s2-buffer (map-into
                    (make-array (array-total-size s2)
                                :element-type '(unsigned-byte 64))
                    #'identity (aops:flatten s2)))
        (dimensions (map '(vector (unsigned-byte 32))
                         #'identity dimensions)))
    (with-pointer-to-vector-data (s2-ptr s2-buffer)
      (with-pointer-to-vector-data (dimensions-ptr dimensions)
        (let ((gpu-image (%%create-image ctx (cffi:null-pointer)
                                         s2-ptr dimensions-ptr (array-rank s2))))
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

(defcfun ("an_image_get_s2" %%image-s2) :int
  (image image)
  (s2    (:pointer :uint64)))

(defun %image-s2 (image dimensions)
  (let* ((s2-dimensions (dimensions-image->s2 dimensions))
         (s2 (make-array s2-dimensions :element-type '(unsigned-byte 64)))
         (s2-buffer (make-array (reduce #'* s2-dimensions) :element-type '(unsigned-byte 64))))
    (with-pointer-to-vector-data (s2-ptr s2-buffer)
      (let ((res (%%image-s2 image s2-ptr)))
        (when (zerop res)
          (error 'recon-error :format-control "Cannot read S₂ from GPU"))))
    (map-into (aops:flatten s2) #'identity s2-buffer)
    s2))
