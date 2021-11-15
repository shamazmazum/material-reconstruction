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

(defmacro muffle-output (&body body)
  `(let ((*standard-output* (make-broadcast-stream)))
     ,@body))

(in-suite annealing)
(test annealing-s2
  (let* ((target-array  (create-image-with-noise 100 100))
         (target-s2     (s2 target-array))
         (initial-array (initialize-random target-array)))
    (with-gpu-objects ((ctx gpu-context)
                       (recon  image-s2  :array initial-array
                                         :context ctx)
                       (target corrfn-s2 :s2 target-s2
                                         :dimensions (array-dimensions target-array)
                                         :context ctx)
                       (proximeter proximeter :target target
                                              :recon recon))
      (let ((cost-state (make-instance 'cost-state
                                       :proximeter proximeter
                                       :target    target
                                       :recon    recon))
            (cooldown   (aarts-korst-cooldown :n 50 :alpha 0.03d0))
            (modifier   (make-modifier)))
        (muffle-output
          (run-annealing target recon 1d-5 10000
                         :cost     (alexandria:curry #'cost cost-state)
                         :cooldown cooldown
                         :modifier modifier))
        (is (< (cost cost-state target recon) 0.9))))))

(test annealing-l2
  (let* ((target-array  (create-image-with-noise 100 100))
         (initial-array (initialize-random target-array))
         (recon  (make-instance 'image-l2 :array initial-array))
         (target (make-instance 'corrfn-l2
                                :l2-void  (l2 target-array 0)
                                :l2-solid (l2 target-array 1)))
         (cost-state (make-instance 'cost-state
                                    :target target
                                    :recon  recon))
         (cooldown   (aarts-korst-cooldown :n 50 :alpha 0.03d0))
         (modifier   (make-modifier)))
    (muffle-output
      (run-annealing target recon 1d-5 10000
                     :cost     (alexandria:curry #'cost cost-state)
                     :cooldown cooldown
                     :modifier modifier))
    (is (< (cost cost-state target recon) 0.9))))

(in-suite l2-update)
(test l2-update-2d
  (let ((image (make-instance
                'image-l2
                :array (make-array '(200 100)
                                   :element-type 'bit :initial-element 0))))
    (loop repeat 50000 do
      (let ((coord (list (random 200)
                         (random 100))))
        (setf (image-pixel image coord)
              (- 1 (image-pixel image coord)))))
    (is (equalp (material-reconstruction::l2-void (image-l2 image))
                (material-reconstruction::l2 (image-array image) 0)))
    (is (equalp (material-reconstruction::l2-solid (image-l2 image))
                (material-reconstruction::l2 (image-array image) 1)))))

(test l2-update-3d
  (let ((image (make-instance
                'image-l2
                :array (make-array '(100 50 25)
                                   :element-type 'bit :initial-element 0))))
    (loop repeat 200000 do
      (let ((coord (list (random 100)
                         (random 50)
                         (random 25))))
        (setf (image-pixel image coord)
              (- 1 (image-pixel image coord)))))
    (is (equalp (material-reconstruction::l2-void (image-l2 image))
                (material-reconstruction::l2 (image-array image) 0)))
    (is (equalp (material-reconstruction::l2-solid (image-l2 image))
                (material-reconstruction::l2 (image-array image) 1)))))
