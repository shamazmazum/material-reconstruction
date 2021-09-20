(in-package :material-reconstruction)

(defun count-foreground-phase (array)
  (reduce #'+ (aops:flatten array)))

;; This is generally for tests
(defun initialize-random (target)
  "Create an array with random data with the same ratio of binary
phases as in @c(target). @c(target) must be a binary 2D array."
  (loop with init           = (make-array (array-dimensions target)
                                          :element-type 'bit
                                          :initial-element 0)
        with phase-target   = (count-foreground-phase target)
        with phase-init     = 0
        with height         = (array-dimension init 0)
        with width          = (array-dimension init 1)
        while (< phase-init phase-target) do
          (let ((x (random width))
                (y (random height)))
            (when (zerop (aref init y x))
              (incf phase-init)
              (setf (aref init y x) 1)))
        finally (return init)))
