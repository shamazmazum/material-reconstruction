(in-package :material-reconstruction)

(defun s2-cost (proximeter)
  (declare (optimize (speed 3))
           (type proximeter proximeter))
  (let ((initial-value (proximity proximeter)))
    (lambda (image1 image2)
      (declare (ignore image1 image2))
      (/ (proximity proximeter) initial-value))))
