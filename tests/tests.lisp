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

(defun test-annealing (steps &key (side 300) (t0 1d-5))
  (let* ((target-array  (create-image-with-noise side side))
         (initial-array (initialize-random target-array)))
    (with-gpu-context (ctx)
      (with-images ((recon  initial-array :context ctx)
                    (target target-array  :context ctx))
        (with-proximeter (proximeter recon target)
          (let ((cost     (s2-cost proximeter))
                (cooldown (aarts-korst-cooldown :n 50 :alpha 0.03d0))
                (modifier (make-modifier)))
            (run-annealing recon target t0 steps
                           :cost     cost
                           :cooldown cooldown
                           :modifier modifier)
            (funcall cost recon target)))))))

(in-suite annealing)
(test annealing
  (let ((*standard-output* (make-string-output-stream)))
    (is (< (test-annealing 10000 :side 100) 0.9))))
