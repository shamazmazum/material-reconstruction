(in-package :material-reconstruction)

(-> different-neighbors ((simple-array bit))
    (values (simple-array (unsigned-byte 8)) &optional))
(defun different-neighbors (array)
  "Return an array which has the same shape as ARRAY. An element of
the returned array with index IDX contains a number of neighbors of
ARRAY[IDX] which are not equal to ARRAY[IDX]."
  (declare (optimize (speed 3)))
  (let ((neighbor-map (make-array (array-dimensions array)
                                  :element-type '(unsigned-byte 8)))
        (dimensions (array-dimensions array))
        (indices (si:imap #'alexandria:flatten
                          (reduce #'si:product
                                  (loop for dim in (array-dimensions array) collect
                                        (si:range 0 dim)))))
        (neighbor-area (si:imap #'alexandria:flatten
                                (reduce #'si:product
                                        (loop repeat (array-rank array) collect
                                              (si:range -1 2))))))
    (si:do-iterator (index indices)
      (let ((element (apply #'aref array index)))
        (declare (type bit element))
        (setf (apply #'aref neighbor-map index)
              (si:foldl #'+ 0
                        (si:imap
                         (lambda (shift)
                           (let ((neighbor
                                  (apply #'aref array
                                         (mapcar
                                          (lambda (i j d)
                                            (declare (type (unsigned-byte 32) i d)
                                                     (type (integer -1 1) j))
                                            (mod (+ i j) d))
                                          index shift dimensions))))
                             (declare (type bit neighbor))
                             (if (= element neighbor) 0 1)))
                         neighbor-area)))))
    neighbor-map))

(-> different-neighbors-hist ((simple-array (unsigned-byte 8)))
    (values (simple-array fixnum (*)) &optional))
(defun different-neighbors-hist (different-neighbors)
  "Count the amount of pixels with a number of different phase
neighbors varying from 0 to 3^D-1."
  (declare (optimize (speed 3)))
  (let* ((length (expt 3 (array-rank different-neighbors)))
         (histogram (make-array length :element-type 'fixnum :initial-element 0)))
    (map nil
         (lambda (count)
           (incf (aref histogram count)))
         (aops:flatten different-neighbors))
    histogram))

(-> update-different-neighbors ((simple-array (unsigned-byte 8))
                                (simple-array bit)
                                list)
    (values list &optional))
(defun update-different-neighbors (different-neighbors array index)
  "Update DIFFERENT-NEIGHBORS map after ARRAY[INDEX] was flipped"
  (declare (optimize (speed 3)))
  (assert (equalp (array-dimensions different-neighbors)
                  (array-dimensions array)))
  (let ((element (apply #'aref array index))
        (max-diff (1- (expt 3
                            (the (integer 1 3)
                                 (array-rank array)))))
        (neighbor-area (si:imap #'alexandria:flatten
                                (reduce #'si:product
                                        (loop repeat (array-rank array) collect
                                              (si:range -1 2))))))
    ;; Update central element
    (setf (apply #'aref different-neighbors index)
          (- max-diff (the (unsigned-byte 8)
                           (apply #'aref different-neighbors index))))

    ;; Update neighbors
    (si:do-iterator (shift neighbor-area)
      (unless (every #'zerop shift)
        (let ((neighbor-index
               (mapcar
                (lambda (i j d)
                  (declare (type (unsigned-byte 32) i d)
                           (type (integer -1 1) j))
                  (mod (+ i j) d))
                index shift (array-dimensions array))))
          (incf (apply #'aref different-neighbors neighbor-index)
                (if (= (the bit element)
                       (the bit (apply #'aref array neighbor-index)))
                    -1 1))))))
  index)
