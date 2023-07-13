(in-package :material-reconstruction)

(declaim (inline neighbors-iterator))
(defun neighbors-iterator (rank)
  (si:imap #'alexandria:flatten
           (reduce #'si:product
                   (loop repeat rank collect (si:range -1 2)))))

(declaim (inline neighbor-index))
(defun neighbor-index (center shift dimensions)
  (mapcar
   (lambda (c s d)
     (declare (type (unsigned-byte 32) c d)
              (type (integer -1 1) s))
     (mod (+ c s) d))
   center shift dimensions))

(defmacro do-neighbors ((index array center) &body body)
  "Bind INDEX an index of each neighbor of the element of ARRAY at
index CENTER and execute BODY for each neighbor."
  (alex:with-gensyms (array-var neighbor-area shift rank dimensions)
    `(let* ((,array-var ,array)
            (,rank (array-rank ,array-var))
            (,dimensions (array-dimensions ,array-var))
            (,neighbor-area (neighbors-iterator ,rank)))
       (si:do-iterator (,shift ,neighbor-area)
         (let ((,index (neighbor-index ,center ,shift ,dimensions)))
           ,@body)))))

(sera:-> neighbors-map ((simple-array bit))
         (values (simple-array (unsigned-byte 8)) &optional))
(defun neighbors-map (array)
  "Return an array which has the same shape as @c(array). An element of
the returned array with index @c(idx) contains a number of neighbors of
@c(array[idx]) which are not equal to @c(array[idx])."
  (declare (optimize (speed 3)))
  (let ((neighbor-map (make-array (array-dimensions array)
                                  :element-type '(unsigned-byte 8)))
        (dimensions (array-dimensions array))
        (indices (si:imap #'alexandria:flatten
                          (reduce #'si:product
                                  (loop for dim in (array-dimensions array) collect
                                        (si:range 0 dim)))))
        (neighbor-area (neighbors-iterator (array-rank array))))
    (si:do-iterator (index indices)
      (let ((element (apply #'aref array index)))
        (declare (type bit element))
        (setf (apply #'aref neighbor-map index)
              (si:foldl #'+ 0
                        (si:imap
                         (lambda (shift)
                           (let ((neighbor
                                  (apply #'aref array (neighbor-index index shift dimensions))))
                             (declare (type bit neighbor))
                             (if (= element neighbor) 0 1)))
                         neighbor-area)))))
    neighbor-map))

(sera:-> neighbors-hist ((simple-array (unsigned-byte 8)))
         (values (simple-array fixnum (*)) &optional))
(defun neighbors-hist (different-neighbors)
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

(sera:-> update-neighbors-map ((simple-array (unsigned-byte 8))
                               (simple-array bit)
                               list)
         (values list &optional))
(defun update-neighbors-map (different-neighbors array index)
  "Update DIFFERENT-NEIGHBORS map after ARRAY[INDEX] was flipped"
  (declare (optimize (speed 3)))
  (assert (equalp (array-dimensions different-neighbors)
                  (array-dimensions array)))
  (let ((element (apply #'aref array index))
        (max-diff (1- (expt 3
                            (the (integer 1 3)
                                 (array-rank array))))))
    ;; Update central element
    (setf (apply #'aref different-neighbors index)
          (- max-diff (the (unsigned-byte 8)
                           (apply #'aref different-neighbors index))))

    ;; Update neighbors
    (do-neighbors (neighbor-index array index)
      (unless (every (lambda (x y)
                       (declare (type fixnum x y))
                       (= x y))
                     neighbor-index index)
        (incf (apply #'aref different-neighbors neighbor-index)
              (if (= (the bit element)
                     (the bit (apply #'aref array neighbor-index)))
                  -1 1)))))
  index)
