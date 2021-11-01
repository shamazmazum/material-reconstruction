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
  "Return iterator which counts runs of X in bit array ARRAY."
  (values #'run-next (cons x array) 0))

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
  "Return interator which enumerates all elements of LIST, starting
with 0."
  (values #'enumerate-next list -1))
