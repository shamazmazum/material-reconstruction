(in-package :material-reconstruction)

(sera:-> count-foreground-phase ((simple-array bit))
         (values alex:non-negative-fixnum &optional))
(defun count-foreground-phase (array)
  (reduce #'+ (aops:flatten array)))

(sera:-> initialize-random ((simple-array bit))
         (values (simple-array bit) &optional))
(defun initialize-random (target)
  "Create an array with random data with the same ratio of binary
phases as in @c(target). @c(target) must be a binary array."
  (loop with init           = (make-array (array-dimensions target)
                                          :element-type 'bit
                                          :initial-element 0)
        with phase-target   = (count-foreground-phase target)
        with phase-init     = 0
        while (< phase-init phase-target) do
          (let ((random-coord (mapcar #'random (array-dimensions target))))
            (when (zerop (apply #'aref init random-coord))
              (incf phase-init)
              (setf (apply #'aref init random-coord) 1)))
        finally (return init)))
