(in-package :material-reconstruction)

(defun exponential-cooldown (&key (n 10000) (alpha 0.95))
  "Return exponential cooldown schedule with decay coefficient
@c(alpha). Annealing temperature @c(temp) decreases @c(alpha) times
each @c(n)-th step."
  (declare (optimize (speed 3))
           (type (integer 1 #.most-positive-fixnum) n)
           (type single-float alpha))
  (let ((count 0))
    (declare (type fixnum count))
    (lambda (temperature energy)
      (declare (type single-float temperature)
               (ignore energy))
      (incf count)
      (if (zerop (rem count n))
          (* temperature alpha)
          temperature))))

(declaim (ftype (function ((simple-array single-float))
                          (values single-float &optional))
                mean std))
(defun mean (array)
  (declare (optimize (speed 3))
           (type (simple-array single-float) array))
  (/ (the single-float (reduce #'+ array))
     (length array)))

(defun std (array)
  (declare (optimize (speed 3))
           (type (simple-array single-float) array))
  (let ((mean (mean array)))
    (sqrt
     (/ (the (single-float 0.0)
             (reduce (lambda (acc x)
                       (declare (type single-float acc x))
                       (+ acc (expt (- x mean) 2)))
                     array
                     :initial-value 0.0))
        (the positive-fixnum
             (1- (length array)))))))

(defun aarts-korst-cooldown (&key (n 10000) (alpha 0.5))
  "Create a cooldown schedule described by Aarts and Korst @b(FIXME:
where?). Annealing temperature decreases once each @c(n) steps
according to a parameter @c(alhpa). The larger that parameter is the
faster the system loses temperature."
  (declare (optimize (speed 3))
           (type (integer 1 #.most-positive-fixnum) n)
           (type single-float alpha))
  (let ((costs (make-array n :element-type 'single-float))
        (counter 0))
    (declare (type (integer 0 #.most-positive-fixnum) counter))
    (lambda (temperature cost)
      (declare (type single-float temperature cost))
      (setf (aref costs counter) cost
            counter (rem (1+ counter) n))
      (if (zerop counter)
          (let ((std (std costs)))
            (/ (* temperature std)
               (+ (* alpha temperature) std)))
          temperature))))
