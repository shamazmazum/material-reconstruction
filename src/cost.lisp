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

(defmethod initialize-instance :after ((cost cost-state)
                                       &key target recon &allow-other-keys)
  (setf (cost-initial cost)
        (image-distance cost target recon)))

(-> cost (cost-state corrfn image)
    (values single-float &optional))
(defun cost (cost target recon)
  "Calculate cost function for the image @c(recon) and the target set
of correlation functions @c(target). @c(cost) is an object of type
@c(cost-state) created for the same @c(recon) and
@c(target). Correlation functions used in the calculation depend on
class of @c(recon) and @c(target).

Function like (alexadria:curry #'cost cost-state) can be used as a
cost function in @c(annealing-step)."
  (let ((differences (image-distance cost target recon))
        (initial-values (cost-initial cost)))
    (reduce
     (lambda (acc diff)
       (+ acc (/ (cdr diff)
                 (cdr (assoc (car diff) initial-values)))))
     differences :initial-value 0.0)))
