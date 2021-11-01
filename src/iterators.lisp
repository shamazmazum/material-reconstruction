(in-package :material-reconstruction)

(defun lua-for% (next state control thunk)
  (declare (type function next thunk))
  (multiple-value-bind (next-control value)
      (funcall next state control)
    (when next-control
      (funcall thunk next-control value)
      (lua-for% next state next-control thunk))))

(defmacro lua-for ((key value form) &body body)
  "Iterate through iterator constructed with FORM"
  (alexandria:with-gensyms (function state control)
    `(multiple-value-bind (,function ,state ,control)
         ,form
       (lua-for% ,function ,state ,control
                  (lambda (,key ,value)
                    ,@body)))))

(defun run-next (state control)
  (declare (optimize (speed 3))
           (type alexandria:non-negative-fixnum control))
  (destructuring-bind (x . array) state
    (declare (type bit x)
             (type (simple-array bit (*)) array))
    (let ((start (position x array :start control)))
      (when start
        (let ((stop (or (position (- 1 x) array :start (1+ start))
                        (length array))))
          (values stop (- stop start)))))))
          
(defun count-runs (array x)
  (values #'run-next (cons x array) 0))

(defun icount-next (state control)
  (declare (optimize (speed 3))
           (type fixnum state control))
  (if (< control state)
      (values (1+ control) control)))

(defun icount (stop &key (start 0))
  (values #'icount-next stop start))

(defun slice (array coord)
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

(defun slices-2d-next (state control)
  (declare (optimize (speed 3))
           (type alexandria:non-negative-fixnum control))
  (destructuring-bind (axis . array)
      state
    (declare (type bit axis))
    (let* ((another-axis (- 1 axis))
           (length (array-dimension array another-axis)))
      (declare (type alexandria:non-negative-fixnum length))
      (if (< control length)
          (values (1+ control)
                  (slice array
                         (if (zerop axis)
                             (list t control)
                             (list control t))))))))

(defun slices-2d (array axis)
  (declare (type bit axis))
  (values #'slices-2d-next (cons axis array) 0))

(defun enumerate-next (state control)
  (declare (optimize (speed 3))
           (type fixnum control))
  (let* ((control (1+ control))
         (elt (nth control state)))
    (if elt (values control elt))))

(defun enumerate (list)
  (values #'enumerate-next list -1))
