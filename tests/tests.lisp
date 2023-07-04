(in-package material-reconstruction-tests)

(def-suite annealing
  :description "Test simulated annealing")
(def-suite s2-update
  :description "Test two-point function updates")
(def-suite different-neighbors-update
  :description "Test different-neighbors map updates")

(defun run-tests ()
  "Run all tests and return T if all tests have passed"
  (every #'identity
         (mapcar (lambda (suite)
                   (let ((status (run suite)))
                     (explain! status)
                     (results-status status)))
                 '(annealing s2-update different-neighbors-update))))

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

(defun create-random-array (side ndims)
  (let ((array (make-array (loop repeat ndims collect side)
                           :element-type 'bit)))
    (aops:each-index (i)
      (setf (row-major-aref array i)
            (random 2)))
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

(in-suite s2-update)
(defun test-s2-update (size ndims periodic)
  (let ((data (create-random-array size ndims)))
    (with-gpu-objects ((ctx gpu-context)
                       (image image-s2
                              :periodic-p periodic
                              :array      data
                              :context    ctx))
      (loop repeat 5000
            for index = (loop repeat ndims collect (random size)) do
            (setf (image-pixel image index)
                  (- 1 (image-pixel image index))))
      (is (equalp (s2 data :periodic-p periodic) (image-gpu-s2 image))))))

(test s2-update-1d-periodic
  (test-s2-update 10000 1 t))

(test s2-update-2d-periodic
  (test-s2-update 1000 2 t))

(test s2-update-3d-periodic
  (test-s2-update 100 3 t))

(test s2-update-1d-non-periodic
  (test-s2-update 10000 1 nil))

(test s2-update-2d-non-periodic
  (test-s2-update 500 2 nil))

(test s2-update-3d-non-periodic
  (test-s2-update 50 3 nil))

(in-suite different-neighbors-update)
(defun test-dn-update (size ndims)
  (let* ((data (create-random-array size ndims))
         (dn (material-reconstruction::different-neighbors data)))
    (loop repeat 5000
          for index = (loop repeat ndims collect (random size)) do
          (setf (apply #'aref data index)
                (- 1 (apply #'aref data index)))
          (material-reconstruction::update-different-neighbors dn data index))
    (is (equalp (material-reconstruction::different-neighbors data) dn))))

(test dn-update-1d
  (test-dn-update 10000 1))

(test dn-update-2d
  (test-dn-update 1000 2))

(test dn-update-3d
  (test-dn-update 100 3))

(defclass test-image (image update-callback-mixin)
  ())

(defun test-dpn-sampler (size ndims)
  (let* ((data (create-random-array size ndims))
         (sampler (make-instance 'dpn-sampler :array data))
         (image (make-instance 'test-image
                               :array data
                               :callback (dpn-update-callback sampler))))
    (loop repeat 5000
          for index = (loop repeat ndims collect (random size)) do
          (setf (image-pixel image index)
                (- 1 (image-pixel image index))))
    (let ((neighbors-map (material-reconstruction::different-neighbors data)))
      (is (equalp neighbors-map (dpn-sampler-neighbors-map sampler)))
      (is (equalp (material-reconstruction::different-neighbors-hist neighbors-map)
                  (dpn-sampler-neighbors-hist sampler))))))

(test dpn-sampler-1d
  (test-dpn-sampler 10000 1))

(test dpn-sampler-2d
  (test-dpn-sampler 1000 2))

(test dpn-sampler-3d
  (test-dpn-sampler 100 3))
