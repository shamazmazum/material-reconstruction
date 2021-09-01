(in-package :material-reconstruction)

(defun exponential-cooldown (&optional (alpha 1d-3))
  (declare (optimize (speed 3))
           (type double-float alpha))
  (lambda (temperature energy)
    (declare (type double-float temperature)
             (ignore energy))
    (* temperature alpha)))
