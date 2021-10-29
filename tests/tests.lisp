(in-package material-reconstruction-tests)

(def-suite annealing
  :description "Test simulated annealing")
(def-suite l2-update
  :description "Test lineal-path function updates")

(defun run-tests ()
  "Run all tests and return T if all tests have passed"
  (every #'identity
         (mapcar (lambda (suite)
                   (let ((status (run suite)))
                     (explain! status)
                     (results-status status)))
                 '(annealing l2-update))))

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

(defun test-annealing-s2 (steps &key (side 300) (t0 1d-5))
  (let* ((target-array  (create-image-with-noise side side))
         (initial-array (initialize-random target-array)))
    (with-gpu-context (ctx)
      (with-images ((recon  initial-array image-s2 :context ctx)
                    (target target-array  image-s2 :context ctx))
        (with-proximeter (proximeter recon target)
          (let ((cost-state (make-instance 'cost-state
                                           :proximeter proximeter
                                           :image-x    recon
                                           :image-y    target))
                (cooldown   (aarts-korst-cooldown :n 50 :alpha 0.03d0))
                (modifier   (make-modifier)))
            (run-annealing recon target t0 steps
                           :cost-state cost-state
                           :cooldown   cooldown
                           :modifier   modifier)
            (cost cost-state recon target)))))))

(defun test-annealing-l2 (steps &key (side 300) (t0 1d-5))
  (let* ((target-array  (create-image-with-noise side side))
         (initial-array (initialize-random target-array)))
    (with-images ((recon  initial-array image-l2)
                  (target target-array  image-l2))
      (let ((cost-state (make-instance 'cost-state
                                       :image-x    recon
                                       :image-y    target))
            (cooldown   (aarts-korst-cooldown :n 50 :alpha 0.03d0))
            (modifier   (make-modifier)))
        (run-annealing recon target t0 steps
                       :cost-state cost-state
                       :cooldown   cooldown
                       :modifier   modifier)
        (cost cost-state recon target)))))

(in-suite annealing)
(test annealing
  (let ((*standard-output* (make-string-output-stream))
        (test-functions (list #'test-annealing-s2 #'test-annealing-l2)))
    (map nil
         (lambda (fn)
           (is (< (funcall fn 10000 :side 100) 0.9)))
         test-functions)))

(in-suite l2-update)
(test l2-update
  (let ((image (create-image
                (make-array '(100 100) :element-type 'bit :initial-element 0)
                'image-l2)))
    (loop repeat 50000 do
      (let ((coord (list (random 100)
                         (random 100))))
        (setf (image-pixel image coord)
              (- 1 (image-pixel image coord)))))
    (is (equalp (material-reconstruction::image-l2-void image)
                (material-reconstruction::l2 (image-array image) 0)))
    (is (equalp (material-reconstruction::image-l2-solid image)
                (material-reconstruction::l2 (image-array image) 1)))))
