(defsystem :material-reconstruction
  :license "BSD 2-Clause"
  :description "Simulated annealing of materials based on S₂ and L₂ correlation functions"
  :author "Vasily Postnicov <shamaz.mazum@gmail.com>"
  :version "0.1"
  :defsystem-depends-on (:material-reconstruction-asdf)
  :depends-on (:cffi
               :array-operations
               :select
               :alexandria
               :serapeum)
  :serial t
  :pathname "src/"
  :components ((c-library "anneal")
               (:file "package")
               #+nil
               (:file "l2")
               (:file "conditions")
               (:file "ffi")
               (:file "gpu-object")
               (:file "gpu-context")
               (:file "corrfns")
               (:file "image")
               (:file "initialization")
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
  :pathname "tests/"
  :components ((:file "package")
               (:file "tests" :depends-on ("package")))
  :depends-on (:material-reconstruction
               :alexandria
               :fiveam
               :cl-value-noise))
