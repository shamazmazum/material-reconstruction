(in-package :material-reconstruction)

(defun dimensions->ranges (dimensions)
  (mapcar
   (lambda (d)
     (select:range 0 d))
   dimensions))

(defun array-indices (dimensions)
  "Return an iterator which iterates through array indices"
  (si:imap #'alexandria:flatten
           (reduce #'si:product
                   (loop for dim in dimensions collect
                         (si:range 0 dim)))))

(-> cut-s2 ((simple-array (unsigned-byte 64)))
    (values (simple-array (unsigned-byte 64)) &optional))
(defun cut-s2 (array)
  "Cut redundant symmetric parts from S₂ array"
  (apply #'select:select
         array
         (dimensions->ranges
          (dimensions-image->s2
           (array-dimensions array)))))

(-> s2 ((simple-array bit))
    (values (simple-array (unsigned-byte 64)) &optional))
(defun s2 (array)
  "Calculate two-point correlation function for a bit array."
  (let* ((floats (aops:vectorize* 'single-float
                     (array)
                   (float array 0.0)))
         (dft (rfft floats))
         (s2-dft (aops:vectorize* '(complex single-float)
                     (dft)
                   (complex (expt (abs dft) 2) 0.0)))
         (s2 (irfft s2-dft (array-dimensions array))))
    (cut-s2
     (aops:vectorize* '(unsigned-byte 64)
         (s2)
       (round (/ s2 (array-total-size array)))))))

(-> shifted-s2 ((simple-array bit))
    (values (simple-array (unsigned-byte 64)) &optional))
(defun shifted-s2 (array)
  "Calculate two-point correlation function for a bit array. Zeroth
correlation length is shifted to the center of S₂ array."
  (unless (every #'evenp (array-dimensions array))
    (error 'recon-error :format-control "All dimensions must be even"))
  (let* ((floats (aops:vectorize* 'single-float
                     (array)
                   (float array 0.0)))
         (dft (rfft floats))
         (s2-dft (make-array (array-dimensions dft)
                             :element-type '(complex single-float))))
    (si:do-iterator (index (array-indices (array-dimensions dft)))
      (setf (apply #'aref s2-dft index)
            (* (complex (expt (abs (apply #'aref dft index)) 2) 0.0)
               (if (evenp (reduce #'+ index)) 1 -1))))
    (let ((s2 (irfft s2-dft (array-dimensions array))))
      (cut-s2
       (aops:vectorize* '(unsigned-byte 64)
           (s2)
         (round (/ s2 (array-total-size array))))))))
