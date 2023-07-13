(in-package :material-reconstruction)

(defclass sampler () ()
  (:documentation "Generic sampler class"))

(defgeneric sample (sampler image)
  (:documentation "Return indices in the image"))

(defclass modifier () ()
  (:documentation "Generic modifier class"))

(defgeneric modify (modifier image &optional recursivep))

(defmethod modify :before ((modifier modifier) image &optional recursivep)
  (unless recursivep
    (image-start-modification image)))

(defclass interface-sampler (sampler) ()
  (:documentation "This sampler takes a sample on a boundary between
two phases"))

(defgeneric random-direction (dimensions)
  (:documentation "Generate unit vector in random direction"))

(declaim (type single-float +pi+))
(defconstant +pi+ (float pi 0.0))

(defmethod random-direction ((dimensions (eql 2)))
  (declare (optimize (speed 3)))
  (let ((ϕ (random (* 2 +pi+))))
    (list (sin ϕ) (cos ϕ))))

(defmethod random-direction ((dimensions (eql 3)))
  (declare (optimize (speed 3)))
  (let ((ϕ (random (* 2 +pi+)))
        (ψ (- (random +pi+) (/ +pi+ 2))))
    (list
     (sin ψ)
     (* (cos ψ) (sin ϕ))
     (* (cos ψ) (cos ϕ)))))

(defmethod sample ((sampler interface-sampler) image)
  (loop with dimensions      = (image-dimensions image)
        with coord           = (mapcar #'random dimensions)
        with delta           = (random-direction (length dimensions))
        with init-val fixnum = (image-pixel image coord)
        do
           (setf coord (mapcar #'+ coord delta))
           (let ((coord (mapcar #'round coord)))
             (cond
               ((some
                 (lambda (x boundary)
                   (or (>= x boundary) (< x 0)))
                 coord dimensions)
                (return-from sample (sample sampler image)))
               ((/= (image-pixel image coord) init-val)
                (return-from sample coord))))))

(defclass uniform-sampler (sampler) ()
  (:documentation "Take samples uniformly over the whole image"))

(defmethod sample ((sampler uniform-sampler) image)
  (mapcar #'random (image-dimensions image)))

(defclass dpn-sampler (sampler)
  ((neighbors-map  :accessor      dpn-sampler-neighbors-map
                   :type          (simple-array (unsigned-byte 8))
                   :documentation "Contains a map of numbers of pixels
lying in the different phase.")
   (neighbors-hist :accessor      dpn-sampler-neighbors-hist
                   :type          (simple-array fixnum (*))
                   :documentation "@c(neighbors-map) binned to a histogram")
   (α              :reader        dpn-sampler-α
                   :type          single-float
                   :initarg       :α
                   :initform      1.0
                   :documentation "A parameter ≥ 1. Higher values of
this parameter results in higher probabilty of choosing pixels with a
big value of neighbors which lie in the different phase. Good values
are 1.4 for 3D images and 2.4 for 2D images."))
  (:documentation "Sampler which prefers elements with higher number
of neighbors lying in the different phase."))

(sera:-> sampling-cdf (dpn-sampler)
         (values (simple-array single-float (*)) &optional))
(defun sampling-cdf (sampler)
  (declare (optimize (speed 3)))
  (let ((α    (dpn-sampler-α sampler))
        (hist (dpn-sampler-neighbors-hist sampler)))
    (declare (type single-float α)
             (type (simple-array fixnum (*)) hist))
    (let* ((length (length hist))
           (cdf  (make-array length :element-type 'single-float)))
      (loop for i fixnum below length
            for x across hist do
            (setf (aref cdf i)
                  (* x (expt α i))))
      (loop for i fixnum from 1 below length do
            (incf (aref cdf i)
                  (aref cdf (1- i))))
      (let ((sum (aref cdf (1- length))))
        (map-into cdf (lambda (x) (/ x sum)) cdf)))))

(defmethod sample ((sampler dpn-sampler) image)
  (declare (optimize (speed 3)))
  (let ((neighbors (dpn-sampler-neighbors-map sampler))
        (dimensions (image-dimensions image)))
    (assert (equalp dimensions (array-dimensions neighbors)))
    (let* ((cdf    (sampling-cdf sampler))
           (random (random 1.0))
           (neighbors-count (position-if (lambda (x) (< random x)) cdf)))
      (labels ((%sample ()
                 (let* ((index (mapcar #'random dimensions))
                        (neighbors-at-index (apply #'aref neighbors index)))
                   (declare (type alex:non-negative-fixnum neighbors-at-index))
                   (if (= neighbors-count neighbors-at-index) index (%sample)))))
        (%sample)))))

(defmethod initialize-instance :after ((sampler dpn-sampler) &key array &allow-other-keys)
  (setf (dpn-sampler-neighbors-map sampler)
        (neighbors-map array)
        (dpn-sampler-neighbors-hist sampler)
        (neighbors-hist
         (dpn-sampler-neighbors-map sampler))))

(sera:-> dpn-update-callback (dpn-sampler)
         (values (sera:-> (image list update-type)
                          (values list &optional))
                 &optional))
(defun dpn-update-callback (sampler)
  "Returns a callback which must be passed as @c(:callback) argument
when creating an image. The class of the image must be a subclass of
@c(update-callback-mixin)."
  (declare (optimize (speed 3)))
  (let ((histogram (dpn-sampler-neighbors-hist sampler))
        (neighbors (dpn-sampler-neighbors-map sampler)))
    (declare (type (simple-array fixnum (*)) histogram))
    (lambda (image index type)
      (case type
        (:pre
         ;; Update histogram
         (do-neighbors (neighbor-index neighbors index)
           (let ((neighbors (apply #'aref neighbors neighbor-index)))
             (decf (aref histogram neighbors)))))
        (:post
         ;; Update neighbor map
         (update-neighbors-map neighbors (image-array image) index)
         ;; Update histogram
         (do-neighbors (neighbor-index neighbors index)
           (let ((neighbors (apply #'aref neighbors neighbor-index)))
             (incf (aref histogram neighbors))))))
      index)))

(defclass flipper (modifier)
  ((sampler :reader   modifier-sampler
            :initarg  :sampler
            :initform (error "Specify sampler")
            :type     sampler))
  (:documentation "A modifier which flips the phase of taken sample
(works with two-phase images only)."))

(defmethod modify ((flipper flipper) image &optional recursivep)
  (declare (optimize (speed 3))
           (ignore recursivep))
  (let ((coord (sample (modifier-sampler flipper) image)))
    (setf (image-pixel image coord)
          (- 1 (image-pixel image coord))))
  image)

(defclass swapper (modifier)
  ((sampler :reader   modifier-sampler
            :initarg  :sampler
            :initform (error "Specify sampler")
            :type     sampler))
  (:documentation "A modifier which swaps two samples with different
phases."))

(defmethod modify ((swapper swapper) image &optional recursivep)
  (declare (optimize (speed 3))
           (ignore recursivep))
  (let* ((coord1 (sample (modifier-sampler swapper) image))
         (coord2 (sample (modifier-sampler swapper) image))
         (sample1 (image-pixel image coord1))
         (sample2 (image-pixel image coord2)))
    (cond
      ((= sample1 sample2)
       (modify swapper image t))
      (t
       (setf (image-pixel image coord1) sample2
             (image-pixel image coord2) sample1)
       image))))

(defclass batch-modifier (modifier)
  ((modifier :initarg  :modifier
             :reader   batch-modifier
             :initform (error "Specify modifier")
             :type     modifier)
   (n        :initarg  :n
             :initform 2
             :reader   batch-modifier-n
             :type     alex:positive-fixnum))
  (:documentation "A modifier which makes @c(n) modifications at a
time with the modifier @c(modifier)."))

(defmethod modify ((modifier batch-modifier) image &optional recursivep)
  (declare (ignore recursivep))
  (loop with base-modifier = (batch-modifier modifier)
        repeat (batch-modifier-n modifier) do
        (modify base-modifier image t))
  image)
