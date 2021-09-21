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

(defmethod sample ((sampler interface-sampler) image)
  (declare (optimize (speed 3)))
  (let ((width (image-width image))
        (height (image-height image)))
    (loop with start-x  single-float = (float (random width) 0f0)
          with start-y  single-float = (float (random height) 0f0)
          with angle    single-float = (random (* 2 (float pi 0f0)))
          with delta-x  single-float = (cos angle)
          with delta-y  single-float = (sin angle)
          with init-val fixnum       = (image-get image
                                                  (round start-x)
                                                  (round start-y))
          do
             (incf start-x delta-x)
             (incf start-y delta-y)
             (let ((x (round start-x))
                   (y (round start-y)))
               (declare (type (signed-byte 32) x y))
               (cond
                 ((or (>= x width)
                      (>= y height)
                      (<  x 0)
                      (<  y 0))
                  (return-from sample (sample sampler image)))
                 ((/= (image-get image x y) init-val)
                  (return-from sample (values x y))))))))

(defmethod sample ((sampler uniform-sampler) image)
  (declare (optimize (speed 3)))
  (let ((width (image-width image))
        (height (image-height image)))
    (values (random width)
            (random height))))

(defclass flipper (modifier)
  ((sampler :reader   modifier-sampler
            :initarg  :sampler
            :initform (error "Specify sampler")
            :type     sampler))
  (:documentation "A modifier which flips the phase of taken sample
(works with two-phase images only)."))

(defmethod modify ((flipper flipper) image)
  (declare (optimize (speed 3)))
  (multiple-value-bind (x y)
      (sample (modifier-sampler flipper) image)
    (declare (type (unsigned-byte 32) x y))
    (image-set image x y
               (- 1 (image-get image x y)))
    (cons x y)))

(defmethod rollback ((flipper flipper) image state)
  (declare (optimize (speed 3)))
  (destructuring-bind (x . y) state
    (declare (type (unsigned-byte 32) x y))
    (image-set image x y (- 1 (image-get image x y)))))

(defclass swapper (modifier)
  ((sampler :reader   modifier-sampler
            :initarg  :sampler
            :initform (error "Specify sampler")
            :type     sampler))
  (:documentation "A modifier which swaps two samples with different
phases."))

(defmethod modify ((swapper swapper) image)
  (declare (optimize (speed 3)))
  (multiple-value-bind (x1 y1)
      (sample (modifier-sampler swapper) image)
    (declare (type (unsigned-byte 32) x1 y1))
    (multiple-value-bind (x2 y2)
        (sample (modifier-sampler swapper) image)
      (declare (type (unsigned-byte 32) x2 y2))
      (let ((sample1 (image-get image x1 y1))
            (sample2 (image-get image x2 y2)))
        (cond
          ((= sample1 sample2)
           (modify swapper image))
          (t
           (image-set image x1 y1 sample2)
           (image-set image x2 y2 sample1)
           (list
            (cons x1 y1)
            (cons x2 y2))))))))

(defmethod rollback ((swapper swapper) image state)
  (declare (optimize (speed 3)))
  (destructuring-bind ((x1 . y1)
                       (x2 . y2))
      state
    (let ((sample1 (image-get image x1 y1))
          (sample2 (image-get image x2 y2)))
      (image-set image x2 y2 sample1)
      (image-set image x1 y1 sample2))))

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
