(in-package :material-reconstruction)

(defun slice (array coord)
  "Take axial slice from the array ARRAY with coordinates COORD. One
element of the list COORD must be T which denotes a free coordinate
which varies in the range
0 - (1- (ARRAY-DIMENSION ARRAY (POSITION T COORD)))."
  (declare (type (simple-array bit) array)
           (type list coord)
           (optimize (speed 3)))
  (let ((first-item  (apply #'array-row-major-index
                      array (substitute 0 t coord :count 1)))
        (second-item (apply #'array-row-major-index
                      array (substitute 1 t coord :count 1)))
        (axis (position t coord)))
    (when (not axis)
      (error "Axis is not specified"))
    (let ((slice (make-array (array-dimension array axis)
                             :element-type 'bit)))
      (loop for i from first-item by (- second-item first-item)
            for j below (length slice) do
              (setf (aref slice j)
                    (row-major-aref array i)))
      slice)))
