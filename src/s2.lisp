(in-package :material-reconstruction)

(defun maximal-shifts (array)
  (mapcar
   (lambda (x) (floor x 2))
   (array-dimensions array)))

(defun s2-ranges (maximal-shifts shifts)
  (loop for m in maximal-shifts
        for s in shifts
        for i from 1 by 1 collect
        (select:range
         (- m s)
         (if (= i (length shifts))
             (1+ m) (+ m s)))))

(defun array-indices (dimensions)
  "Return an iterator which iterates through array indices"
  (si:imap #'alexandria:flatten
           (reduce #'si:product
                   (loop for dim in dimensions collect
                         (si:range 0 dim)))))

(-> cut-s2 ((simple-array (unsigned-byte 64)) list)
    (values (simple-array (unsigned-byte 64)) &optional))
(defun cut-s2 (array shifts)
  "Cut redundant symmetric parts from S₂ array"
  (unless (every #'<= shifts (maximal-shifts array))
    (error 'recon-error
           :format-control "Shifts ~a are too big"
           :format-arguments (list shifts)))
  (apply #'select:select
         array
         (s2-ranges
          (maximal-shifts array)
          shifts)))

(-> s2 ((simple-array bit) &optional list)
    (values (simple-array (unsigned-byte 64)) &optional))
(defun s2 (array &optional (shifts (maximal-shifts array)))
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
         (round (/ s2 (array-total-size array))))
       shifts))))
