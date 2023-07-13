(in-package :material-reconstruction)

(sera:-> s2-dft ((simple-array bit))
         (values (simple-array single-float) &optional))
(defun s2-dft (array)
  "Calculate two-point correlation function for the solid phase of the
bit-array @c(array). The result is not normalized and in frequency
domain."
  (let ((fft (rfft array)))
    (aops:vectorize* 'single-float
        (fft)
      (expt (abs fft) 2))))

(declaim (inline dimensions->ranges))
(defun dimensions->ranges (dimensions)
  (mapcar (lambda (n) (select:range 0 n)) dimensions))

(sera:-> s2-from-dft ((simple-array (complex single-float)) list)
         (values (simple-array fixnum) &optional))
(defun s2-from-dft (dft dimensions)
  "Calculate autocorrelation from the image in frequency domain. The
resulting autocorrelation is in space domain and not normalized."
  (let* ((s2-dft (aops:vectorize* '(complex single-float)
                     (dft)
                   (* dft (conjugate dft))))
         (s2 (irfft s2-dft dimensions))
         (total-size (reduce #'* dimensions)))
    (apply #'select:select
           (aops:vectorize* 'fixnum
               (s2)
             (round (/ s2 total-size)))
           (dimensions->ranges
            (rfft-array-dimensions dimensions)))))

;; S₂ in spatial domain used in tests + zero-padding for non-periodic S₂
(declaim (inline dimensions-with-padding))
(defun dimensions-with-padding (dimensions)
  (mapcar (lambda (x) (1- (* 2 x))) dimensions))

(sera:-> pad-with-zeros ((simple-array bit))
         (values (simple-array bit) &optional))
(defun pad-with-zeros (array)
  (let* ((dimensions (array-dimensions array))
         (new-dimensions (dimensions-with-padding dimensions))
         (ranges (mapcar (lambda (n) (select:range 0 n)) dimensions))
         (result (make-array new-dimensions :element-type 'bit :initial-element 0)))
    (setf (apply #'select:select result ranges) array)
    result))

(sera:-> s2 ((simple-array bit) &key (:periodic-p boolean))
         (values (simple-array fixnum) &optional))
(defun s2 (array &key (periodic-p t))
  "Calculate two-point correlation function (autocorrelation) for the
solid phase of the bit-array @c(array). The result is not normalized."
  (let ((array (if periodic-p array (pad-with-zeros array))))
    (s2-from-dft (rfft array) (array-dimensions array))))
