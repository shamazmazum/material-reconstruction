(in-package :material-reconstruction)

(defclass cost-state ()
  ((initial-cost :type     list
                 :accessor cost-initial))
  (:documentation "An instance of this object is required for
calculation of a cost function. @c(:target) is the target set of
correlation functions and @c(:recon) is an image under
reconstruction."))

(defgeneric image-distance (cost target recon)
  (:documentation "Unscaled difference between images accroding to
some metric")
  (:method-combination list))

(defmethod image-distance list ((cost cost-state)
                                (target corrfn-s2)
                                (recon  image-s2))
  (cons :s2
        (sqrt
         (%distance (object-sap target)
                    (object-sap recon)))))

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

(defmethod initialize-instance :after ((cost cost-state)
                                       &key target recon &allow-other-keys)
  (setf (cost-initial cost)
        (image-distance cost target recon)))

(defun cost (cost target recon)
  "Calculate cost function for the image @c(recon) and the target set
of correlation functions @c(target). @c(cost) is an object of type
@c(cost-state) created for the same @c(recon) and
@c(target). Correlation functions used in the calculation depend on
class of @c(recon) and @c(target).

Function like (alexadria:curry #'cost cost-state) can be used as a
cost function in @c(annealing-step)."
  (declare (type cost-state cost)
           (type image recon)
           (type corrfn target))
  (let ((differences (image-distance cost target recon))
        (initial-values (cost-initial cost)))
    (reduce
     (lambda (acc diff)
       (+ acc (/ (cdr diff)
                 (cdr (assoc (car diff) initial-values)))))
     differences :initial-value 0d0)))
