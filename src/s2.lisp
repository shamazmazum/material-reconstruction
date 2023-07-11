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

(-> s2-from-dft ((simple-array (complex single-float)) list)
    (values (simple-array fixnum) &optional))
(defun s2-from-dft (dft dimensions)
  "Calculate autocorrelation from the image in frequency domain. The
resulting autocorrelation is in space domain and not normalized."
  (let* ((s2-dft (aops:vectorize* '(complex single-float)
                     (dft)
                   (* dft (conjugate dft))))
         (s2 (irfft s2-dft dimensions))
         (total-size (reduce #'* dimensions)))
    (aops:vectorize* 'fixnum
        (s2)
      (round (/ s2 total-size)))))

(-> s2 ((simple-array bit))
    (values (simple-array fixnum) &optional))
(defun s2 (array)
  "Calculate two-point correlation function (autocorrelation) for the
solid phase of the bit-array @c(array). The result is not normalized."
  (s2-from-dft (rfft array) (array-dimensions array)))
