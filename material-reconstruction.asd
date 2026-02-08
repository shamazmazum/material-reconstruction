(defsystem :material-reconstruction
  :license "BSD 2-Clause"
  :description "Simulated annealing of materials based on S₂ and L₂ correlation functions"
  :author "Vasily Postnicov <shamaz.mazum@gmail.com>"
  :version "0.1"
  :depends-on (:cffi
               :stateless-iterators
               :select
               :array-operations
               :alexandria
               :serapeum)
  :serial t
  :pathname "src"
  :components ((:file "package")
               (:file "conditions")
               (:file "ffi")
               (:file "s2")
               (:file "gpu-object")
               (:file "gpu-context")
               (:file "corrfns")
               (:file "image")
               (:file "metric")
               (:file "initialization")
               (:file "neighbors-histogram")
               (:file "samplers")
               (:file "cooldown")
               (:file "cost")
               (:file "annealing"))
  :in-order-to ((test-op (load-op "material-reconstruction/tests")))
  :perform (test-op (op system)
                    (declare (ignore op system))
                    (uiop:symbol-call :material-reconstruction-tests '#:run-tests)))

(defsystem :material-reconstruction/tests
  :version "0.1"
  :author "Vasily Postnicov <shamaz.mazum@gmail.com>"
  :pathname "tests"
  :components ((:file "package")
               (:file "tests" :depends-on ("package")))
  :depends-on (:material-reconstruction
               :alexandria
               :fiveam
               :cl-value-noise))

;; For qlot
(defsystem :material-reconstruciton/docs
  :depends-on (:material-reconstruction :codex))
