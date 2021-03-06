(in-package :material-reconstruction)

(-> l2-slice
    ((simple-array bit (*)) bit)
    (values (simple-array non-negative-fixnum (*)) &optional))
(defun l2-slice (array val)
  (declare (optimize (speed 3))
           (type (simple-array bit (*)) array)
           (type bit val))
  (let ((l2 (make-array (length array)
                        :element-type 'non-negative-fixnum
                        :initial-element 0))
        (first-run 0)
        (last-run  0))
    (declare (type non-negative-fixnum first-run last-run))
    (lua-for (_ runs (count-runs array val))
      (declare (ignore _))
      (setq last-run runs
            first-run (if (zerop first-run) runs first-run))
      (dotimes (i runs)
        (incf (aref l2 i)
              (- runs i))))

    ;; Periodic computations
    (when (= val (aref array 0) (aref array (1- (length array))))
      (let ((sum (+ first-run last-run)))
        (dotimes (i (min sum (length array)))
          (incf (aref l2 i)
                (min i first-run last-run (- sum i))))))
    l2))

(-> l2 ((simple-array bit) bit) (values list &optional))
(defun l2 (array val)
  "Calculate lineal-path function for bit-array @c(array). @c(val) is
a phase for which lineal-path function is calculated. The result is
not normalized."
  (declare (optimize (speed 3))
           (type (simple-array bit) array))
  (let ((dimensions (array-dimensions array)))
    (loop for axis below (length dimensions)
          collect
          (let ((l2 (make-array (reduce #'max dimensions)
                                :element-type 'non-negative-fixnum)))
            (lua-for (_ slice (slices array axis))
              (declare (ignore _))
              (map-into
               l2 #'+ l2 (l2-slice slice val)))
            l2))))
