(in-package :material-reconstruction)

(defun s2 (array)
  "Calculate two-point correlation function for the solid phase of the
bit-array @c(array). The result is not normalized and in frequency
domain."
  (declare (type (simple-array bit) array))
  (let* ((fft (rfft array))
         (s2  (make-array (array-dimensions fft)
                          :element-type 'single-float)))
    (map-into (aops:flatten s2)
              (lambda (x) (expt (abs x) 2))
              (aops:flatten fft))
    s2))
