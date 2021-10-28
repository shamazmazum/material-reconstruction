(in-package :l2)

(defun lua-for% (next state control thunk)
  (declare (type function next thunk))
  (multiple-value-bind (next-control value)
      (funcall next state control)
    (when next-control
      (funcall thunk next-control value)
      (lua-for% next state next-control thunk))))

(defmacro lua-for ((key value form) &body body)
  "Iterate through iterator constructed with FORM"
  (with-gensyms (function state control)
    `(multiple-value-bind (,function ,state ,control)
         ,form
       (lua-for% ,function ,state ,control
                  (lambda (,key ,value)
                    ,@body)))))

(defun run-next (state control)
  (declare (optimize (speed 3))
           (type non-negative-fixnum control))
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

(defun slices-2d-next (state control)
  (declare (optimize (speed 3))
           (type non-negative-fixnum control))
  (destructuring-bind (axis . array)
      state
    (declare (type bit axis))
    (let* ((another-axis (- 1 axis))
           (length (array-dimension array another-axis)))
      (declare (type non-negative-fixnum length))
      (if (< control length)
          (values (1+ control)
                  (apply #'select array
                         (if (zerop axis)
                             (list t control)
                             (list control t))))))))

(defun slices-2d (array axis)
  (declare (type bit axis))
  (values #'slices-2d-next (cons axis array) 0))
