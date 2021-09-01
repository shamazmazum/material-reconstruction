(in-package :material-reconstruction)

(defun annealing-step (system target temp &key cost modifier cooldown)
  (declare (optimize (speed 3))
           (type image system target)
           (type double-float temp)
           (type function cost modifier cooldown))

  (let ((cost1 (funcall cost system target))
        (state (modify modifier system))
        (cost2 (funcall cost system target))
        accepted rejected)
    (declare (type double-float cost1 cost2))
    (when (> cost2 cost1)
      ;; Should we accept the change?
      (let ((random (random 1d0))
            (threshold (exp (/ (- (- cost2 cost1)) temp))))
        (when (> random threshold)
          (rollback modifier system state)
          (setq rejected t)))
      (setq accepted (not rejected)))

    (values
     (if rejected temp (funcall cooldown temp cost2))
     accepted rejected)))

(defun run-annealing (system target t0 n &key cost modifier cooldown)
  (let ((temp t0)
        (rejected 0)
        (accepted 0))
    (loop for steps below n do
      (multiple-value-bind (new-temp step-acc step-rej)
          (annealing-step system target temp
                          :cost     cost
                          :modifier modifier
                          :cooldown cooldown)
        (setq temp new-temp)
        (incf rejected (if step-rej 1 0))
        (incf accepted (if step-acc 1 0)))
      (when (zerop (rem steps 1000))
        (format t "~d steps, ~d accepted, ~d rejected, ~f temp, ~f cost~%"
                steps accepted rejected temp
                (funcall cost system target))))))
