(in-package :material-reconstruction)

(defclass cost ()
  ((initial-cost :type     list
                 :accessor initial-cost))
  (:documentation "An instance of this object is required for
calculation of a cost function."))

(defgeneric image-distance (cost)
  (:documentation "Unscaled difference between images accroding to
some metric")
  (:method-combination list))

(defmethod initialize-instance :after ((cost cost) &rest initargs)
  (declare (ignore initargs))
  (setf (initial-cost cost)
        (image-distance cost)))

(defclass cost-s2 (cost)
  ((metric :type          metric-s2
           :initform      (error "Specify metric object")
           :initarg       :metric
           :reader        cost-s2-metric
           :documentation "@c(metric-s2) object for calculation of
difference in autocorrelation."))
  (:documentation "Objects of this class measure cost based on
decrease in difference between autocorrelation functions. These
objects are backed by @c(metric-s2) objects which must be firstly
created in GPU context."))

(defmethod image-distance list ((cost cost-s2))
  (cons :s2 (sqrt (metric-s2 (cost-s2-metric cost)))))

(sera:-> cost (cost)
         (values single-float &optional))
(defun cost (cost)
  "Calculate cost function for the cost object @c(cost)."
  (let ((current-costs (image-distance cost))
        (initial-values (initial-cost cost)))
    (reduce
     (lambda (acc diff)
       (+ acc (/ (cdr diff)
                 (cdr (assoc (car diff) initial-values)))))
     current-costs :initial-value 0.0)))
