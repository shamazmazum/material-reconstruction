(in-package :material-reconstruction)

(defun annealing-step (target recon temp &key cost modifier cooldown)
  "Perform an annealing step. An annealing procedure modifies
@c(system) minimising cost function @c(cost). @c(cost) is a function
which takes two @c(image) objects and returns a real number. It
usually takes into account one or two correlation functions. For more
information, see documentation abount function with the same name
@c(cost).

Modifications to the system are controlled by @c(modifier)
argument. Currently implemented modifiers are @c(flipper), @c(swapper)
and @c(batch-modifier). A modifier needs a sampler to take samples
from the system. Two implemented samplers are @c(interface-sampler)
and @c(uniform-sampler).

A modification is either accepted or rejected by the procedure. A
parameter @c(temp) is temperature of the system at the current
step. Each time a modification to the system is accepted its
temperature is decreased according to cooldown schedule
@c(cooldown). Currently implemented schedules are
@c(exponential-cooldown) and @c(aarts-korst-cooldown).

This function returns four values: a new value of the system's
temperature, a new value of the cost function, a boolean value which
indicates that a modification to the system has resulted in an
increase of the cost function and another boolean value which
indicates if a modification was discarded."
  (declare (optimize (speed 3))
           (type image recon)
           (type corrfn target)
           (type double-float temp)
           (type function cooldown cost)
           (type modifier modifier))

  (let ((cost1 (funcall cost target recon))
        (state (modify modifier recon))
        (cost2 (funcall cost target recon))
        accepted rejected)
    (declare (type double-float cost1 cost2))
    (when (> cost2 cost1)
      ;; Should we accept the change?
      (let ((random (random 1d0))
            (threshold (exp (/ (- (- cost2 cost1)) temp))))
        (when (> random threshold)
          (rollback modifier recon state)
          (setq rejected t)))
      (setq accepted (not rejected)))

    (values
     (funcall cooldown temp cost2)
     (if rejected cost1 cost2)
     accepted rejected)))

(defun run-annealing (target recon t0 n &key cost modifier cooldown)
  "Run simulated annealing with starting temperature @c(t0) for @c(n)
steps. See also @c(annealing-step)."
  (let ((temp t0)
        (rejected 0)
        (accepted 0)
        cost-list)
    (loop for steps below n do
      (multiple-value-bind (new-temp step-cost step-acc step-rej)
          (annealing-step target recon temp
                          :cost     cost
                          :modifier modifier
                          :cooldown cooldown)
        (push step-cost cost-list)
        (setq temp new-temp)
        (incf rejected (if step-rej 1 0))
        (incf accepted (if step-acc 1 0))

        (when (zerop (rem steps 1000))
          (format t "~d steps, ~d accepted, ~d rejected, ~f temp, ~f cost~%"
                  steps accepted rejected temp step-cost))))
    (reverse cost-list)))
