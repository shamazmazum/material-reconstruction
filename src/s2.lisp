(in-package :material-reconstruction)

(-> s2 ((simple-array bit))
    (values (simple-array single-float) &optional))
(defun s2 (array)
  "Calculate two-point correlation function for the solid phase of the
bit-array @c(array). The result is not normalized and in frequency
domain."
  (let ((fft (rfft array)))
    (aops:vectorize* 'single-float
        (fft)
      (expt (abs fft) 2))))
