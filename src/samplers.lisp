(in-package :material-reconstruction)

(defclass sampler () ()
  (:documentation "Generic sampler class"))

(defgeneric sample (sampler image)
  (:documentation "Return indices in the image"))

(defclass modifier ()
  ((sampler :reader   modifier-sampler
            :initarg :sampler
            :initform (error "Specify sampler")
            :type     sampler))
  (:documentation "Generic modifier class"))

(defgeneric modify (modifier image))
(defgeneric rollback (modifier image state))

(defclass interface-sampler (sampler)
  ())

(defmethod sample ((sampler interface-sampler) image)
  (declare (optimize (speed 3)))
  (let ((width (image-width image))
        (height (image-height image)))
    (loop while t do
      (loop with start-x  single-float = (float (random width) 0f0)
            with start-y  single-float = (float (random height) 0f0)
            with angle    single-float = (random (* 2 (float pi 0f0)))
            with delta-x  single-float = (cos angle)
            with delta-y  single-float = (sin angle)
            with init-val double-float = (image-get image
                                                    (round start-x)
                                                    (round start-y))
            do
               (incf start-x delta-x)
               (incf start-y delta-y)
               (let ((x (round start-x))
                     (y (round start-y)))
                 (declare (type (unsigned-byte 32) x y))
                 (cond
                   ((or (>= x width)
                        (>= y height))
                    (return))
                   ((/= (image-get image x y) init-val)
                    (return-from sample (values x y)))))))))

(defclass flipper (modifier)
  ())

(defmethod modify ((flipper flipper) image)
  (declare (optimize (speed 3)))
  (multiple-value-bind (x y)
      (sample (modifier-sampler flipper) image)
    (declare (type (unsigned-byte 32) x y))
    (image-set image x y
               (- 1d0 (image-get image x y)))
    (cons x y)))

(defmethod rollback ((flipper flipper) image state)
  (declare (optimize (speed 3)))
  (destructuring-bind (x . y) state
    (declare (type (unsigned-byte 32) x y))
    (image-set image x y (- 1d0 (image-get image x y)))))