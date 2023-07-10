(in-package :material-reconstruction)

(defclass sampler () ()
  (:documentation "Generic sampler class"))

(defgeneric sample (sampler image)
  (:documentation "Return indices in the image"))

(defclass modifier () ()
  (:documentation "Generic modifier class"))

(defgeneric modify (modifier image))
(defgeneric rollback (modifier image state))

(defclass interface-sampler (sampler) ()
  (:documentation "This sampler takes a sample on a boundary between
two phases"))

(defclass uniform-sampler (sampler) ()
  (:documentation "Take samples uniformly over the whole image"))

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

(defmethod sample ((sampler uniform-sampler) image)
  (mapcar #'random (image-dimensions image)))

(defclass flipper (modifier)
  ((sampler :reader   modifier-sampler
            :initarg  :sampler
            :initform (error "Specify sampler")
            :type     sampler))
  (:documentation "A modifier which flips the phase of taken sample
(works with two-phase images only)."))

(defmethod modify ((flipper flipper) image)
  (declare (optimize (speed 3)))
  (let ((coord (sample (modifier-sampler flipper) image)))
    (setf (image-pixel image coord)
          (- 1 (image-pixel image coord)))
    coord))

(defmethod rollback ((flipper flipper) image state)
  (declare (optimize (speed 3)))
  (setf (image-pixel image state)
        (- 1 (image-pixel image state))))

(defclass swapper (modifier)
  ((sampler :reader   modifier-sampler
            :initarg  :sampler
            :initform (error "Specify sampler")
            :type     sampler))
  (:documentation "A modifier which swaps two samples with different
phases."))

(defmethod modify ((swapper swapper) image)
  (declare (optimize (speed 3)))
  (let* ((coord1 (sample (modifier-sampler swapper) image))
         (coord2 (sample (modifier-sampler swapper) image))
         (sample1 (image-pixel image coord1))
         (sample2 (image-pixel image coord2)))
    (cond
      ((= sample1 sample2)
       (modify swapper image))
      (t
       (setf (image-pixel image coord1) sample2
             (image-pixel image coord2) sample1)
       (list coord1 coord2)))))

(defmethod rollback ((swapper swapper) image state)
  (destructuring-bind (coord1 coord2)
      state
    (let ((sample1 (image-pixel image coord1))
          (sample2 (image-pixel image coord2)))
      (setf (image-pixel image coord2) sample1
            (image-pixel image coord1) sample2))))

(defclass batch-modifier (modifier)
  ((modifier :initarg  :modifier
             :reader   batch-modifier
             :initform (error "Specify modifier")
             :type     modifier)
   (n        :initarg  :n
             :initform 2
             :reader   batch-modifier-n
             :type     (integer 0 #.most-positive-fixnum)))
  (:documentation "A modifier which makes @c(n) modifications at a
time with the modifier @c(modifier)."))

(defmethod modify ((modifier batch-modifier) image)
  (loop with base-modifier = (batch-modifier modifier)
        repeat (batch-modifier-n modifier) collect
        (modify base-modifier image)))

(defmethod rollback ((modifier batch-modifier) image state)
  (declare (type list state))
  (loop with base-modifier = (batch-modifier modifier)
        for s in (reverse state) do
          (rollback base-modifier image s)))
