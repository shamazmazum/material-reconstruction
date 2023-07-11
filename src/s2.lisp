(in-package :material-reconstruction)

(-> s2-dft ((simple-array bit))
    (values (simple-array single-float) &optional))
(defun s2-dft (array)
  "Calculate two-point correlation function for the solid phase of the
bit-array @c(array). The result is not normalized and in frequency
domain."
  (let ((fft (rfft array)))
    (aops:vectorize* 'single-float
        (fft)
      (expt (abs fft) 2))))

(-> s2 ((simple-array bit))
    (values (simple-array fixnum) &optional))
(defun s2 (array)
  "Calculate two-point correlation function (autocorrelation) for the
solid phase of the bit-array @c(array). The result is not normalized."
  (let* ((fft (rfft array))
         (s2-dft (aops:vectorize* '(complex single-float)
                     (fft)
                   (* fft (conjugate fft))))
         (s2 (irfft s2-dft (array-dimensions array))))
    (aops:vectorize* 'fixnum
        (s2)
      (round (/ s2 (array-total-size array))))))
