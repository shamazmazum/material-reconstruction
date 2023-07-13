(in-package :material-reconstruction)

(sera:-> exponential-cooldown (&key (:n alex:positive-fixnum)
                                    (:α single-float))
         (values (sera:-> (single-float single-float)
                          (values single-float &optional))
                 &optional))
(defun exponential-cooldown (&key (n 10000) (α 0.95f0))
  "Return exponential cooldown schedule with decay coefficient
@c(α). Annealing temperature @c(temp) decreases @c(α) times
each @c(n)-th step."
  (declare (optimize (speed 3)))
  (let ((count 0))
    (declare (type fixnum count))
    (lambda (temperature energy)
      (declare (type single-float temperature)
               (ignore energy))
      (incf count)
      (if (zerop (rem count n))
          (* temperature α)
          temperature))))

(sera:-> mean ((simple-array single-float (*)))
         (values single-float &optional))
(defun mean (array)
  (declare (optimize (speed 3)))
  (/ (reduce #'+ array :initial-value 0.0)
     (length array)))

(sera:-> std ((simple-array single-float (*)))
         (values single-float &optional))
(defun std (array)
  (declare (optimize (speed 3)))
  (let ((mean (mean array)))
    (sqrt
     (/ (reduce
         (lambda (acc x)
           (declare (type single-float acc x))
           (+ acc (expt (- x mean) 2)))
         array
         :initial-value 0f0)
        (1- (length array))))))

(sera:-> aarts-korst-cooldown (&key (:n alex:positive-fixnum)
                                    (:α single-float))
         (values (sera:-> (single-float single-float)
                          (values single-float &optional))
                 &optional))
(defun aarts-korst-cooldown (&key (n 10000) (α 1.0))
  "Create a cooldown schedule described by Aarts and Korst @b(FIXME:
where?). Annealing temperature decreases once each @c(n) steps
according to a parameter @c(alhpa). The larger that parameter is the
faster the system loses temperature."
  (declare (optimize (speed 3)))
  (let ((costs (make-array n :element-type 'single-float))
        (counter 0))
    (declare (type alex:non-negative-fixnum counter))
    (lambda (temperature cost)
      (declare (type single-float temperature cost))
      (setf (aref costs counter) cost
            counter (rem (1+ counter) n))
      (if (zerop counter)
          (let ((std (std costs)))
            (/ (* temperature std)
               (+ (* α temperature) std)))
          temperature))))
