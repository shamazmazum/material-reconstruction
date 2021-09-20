(in-package :material-reconstruction)

(defun s2-cost (proximeter &optional (start 1d0))
  "Create a cost function based on two-point correlation function. An
early created @c(proximeter) object is required to actually measure
the cost."
  (declare (optimize (speed 3))
           (type proximeter proximeter)
           (type double-float start))
  (let ((initial-value (proximity proximeter)))
    (lambda (image1 image2)
      (declare (ignore image1 image2))
      (* start (/ (proximity proximeter) initial-value)))))
