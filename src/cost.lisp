(in-package :material-reconstruction)

(defclass cost-state ()
  ((proximeter    :initarg       :proximeter
                  :reader        cost-proximeter
                  :type          proximeter)
   (initial-cost  :type          list
                  :accessor      cost-initial))
  (:documentation "An instance of this object is required for
calculation of a cost function. @c(:image-x) and @c(:image-y) are two
images which must be specified to calculate the weights for
correlation functions. @c(proximeter) object must be specified when
@c(image-x) and @c(image-y) are of type @c(image-s2)."))

(defgeneric image-distance (cost image-x image-y)
  (:documentation "Unscaled difference between images accroding to
some metric")
  (:method-combination list))

(defmethod image-distance list ((cost cost-state)
                                (image-x image-s2)
                                (image-y image-s2))
  (cons :s2 (proximity (cost-proximeter cost))))

(-> euclidean-distance
    ((simple-array non-negative-fixnum (*))
     (simple-array non-negative-fixnum (*)))
    (values double-float &optional))
(defun euclidean-distance (vector1 vector2)
  (declare (optimize (speed 3))
           (type (simple-array non-negative-fixnum (*)) vector1 vector2))
  (reduce
   #'+
   (map-into (make-array (length vector1) :element-type 'double-float)
             (lambda (x y)
               (declare (type fixnum x y))
               (expt (float (- x y) 0d0) 2))
             vector1 vector2)))

(defmethod image-distance list ((cost cost-state)
                                (image-x image-l2)
                                (image-y image-l2))
  (cons :l2
        (+
         (reduce #'+ (mapcar #'euclidean-distance
                             (image-l2-void image-x)
                             (image-l2-void image-y)))
         (reduce #'+ (mapcar #'euclidean-distance
                             (image-l2-solid image-x)
                             (image-l2-solid image-y))))))

(defmethod initialize-instance :after ((cost cost-state)
                                       &rest initargs
                                       &key image-x image-y &allow-other-keys)
  (declare (ignore initargs))
  (setf (cost-initial cost)
        (image-distance cost image-x image-y)))

(defun cost (cost image-x image-y)
  "Calculate cost function for images @c(image-x) and
@c(image-y). @c(cost) is an object of type @c(cost-state) created for
these two images. Correlation functions used in the calculation depend
on class of @c(image-x) and @c(image-y) arguments which can be either
@c(image-l2), @c(image-s2) or @c(image-all).

Function like (alexadria:curry #'cost cost-state) can be used as a
cost function in @c(annealing-step)."
  (declare (type cost-state cost)
           (type image image-x image-y))
  (let ((differences (image-distance cost image-x image-y))
        (initial-values (cost-initial cost)))
    (reduce
     (lambda (acc diff)
       (+ acc (/ (cdr diff)
                 (cdr (assoc (car diff) initial-values)))))
     differences :initial-value 0d0)))
