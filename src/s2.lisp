(in-package :material-reconstruction)

(defun dimensions->ranges (dimensions)
  (mapcar
   (lambda (d)
     (select:range 0 d))
   dimensions))

(-> s2 ((simple-array bit))
    (values (simple-array (unsigned-byte 64)) &optional))
(defun s2 (array)
  (let* ((floats (aops:vectorize* 'single-float
                     (array)
                   (float array 0.0)))
         (dft (rfft floats))
         (s2-dft (aops:vectorize* '(complex single-float)
                     (dft)
                   (complex (expt (abs dft) 2) 0.0)))
         (s2 (irfft s2-dft (array-dimensions array))))
    (apply #'select:select
           (aops:vectorize* '(unsigned-byte 64)
               (s2)
             (round (/ s2 (array-total-size array))))
           (dimensions->ranges
            (dimensions-image->s2
             (array-dimensions array))))))
