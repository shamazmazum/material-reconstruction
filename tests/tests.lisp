(in-package material-reconstruction-tests)

(def-suite annealing
  :description "Test simulated annealing")

(defun run-tests ()
  "Run all tests and return T if all tests have passed"
  (every #'identity
         (mapcar (lambda (suite)
                   (let ((status (run suite)))
                     (explain! status)
                     (results-status status)))
                 '(annealing))))

(defun create-image-with-noise (h w)
  (let ((array (make-array (list h w) :element-type 'bit)))
    (array-operations/utilities:nested-loop (i j)
        (array-dimensions array)
      (setf (aref array i j)
            (if (< (value-noise (float i) (float j) 0.0
                                :seed (random 20000))
                   0.5)
                0 1)))
    array))

(defun make-modifier ()
  (make-instance 'batch-modifier
                 :modifier (make-instance 'flipper
                                          :sampler (make-instance 'interface-sampler))))

(defmacro muffle-output (&body body)
  `(let ((*standard-output* (make-broadcast-stream)))
     ,@body))

(in-suite annealing)
(test annealing-s2
  (let* ((target-array  (create-image-with-noise 100 100))
         (initial-array (initialize-random target-array)))
    (with-gpu-objects ((ctx gpu-context)
                       (recon  image-s2  :array initial-array
                                         :context ctx)
                       (target corrfn-s2 :array target-array
                                         :context ctx))
      (let ((cost-state (make-instance 'cost-state
                                       :target target
                                       :recon  recon))
            (cooldown   (aarts-korst-cooldown :n 50 :α 1.0))
            (modifier   (make-modifier)))
        (muffle-output
          (run-annealing target recon 1f-5 10000
                         :cost     (alexandria:curry #'cost cost-state)
                         :cooldown cooldown
                         :modifier modifier))
        (is (< (cost cost-state target recon) 0.9))))))
