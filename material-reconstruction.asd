(defclass c->so (asdf:source-file)
  ()
  (:default-initargs
   :type "c"))

(defmethod output-files ((operation compile-op) (component c->so))
  (values
   (list (make-pathname :name "libanneal-ocl"
                        :type #-darwin "so" #+darwin "dylib"
                        :defaults (component-pathname component)))
   t))

(defmethod perform ((operation load-op) (component c->so))
  t)

(defmethod perform ((operation compile-op) (component c->so))
  (flet ((nn (x) (uiop:native-namestring x)))
    (let* ((c-file (component-pathname component))
           (shared-object (make-pathname :type #+darwin "dylib" #-darwin "so"
                                         :name "libanneal-ocl"
                                         :defaults c-file)))
      (uiop:run-program
       (list "cc" "-fPIC" "-shared" "-I/usr/local/include" "-L/usr/local/lib"
             "-o"
             (nn shared-object)
             (nn c-file)
             "-lm" "-lOpenCL" "-lfftw3")))))

(defsystem :material-reconstruction
  :license "BSD 2-Clause"
  :description "Simulated annealing of materials based on S₂ correlation function"
  :author "Vasily Postnicov <shamaz.mazum@gmail.com>"
  :version "0.1"
  :depends-on (:cffi :array-operations)
  :serial t
  :pathname "src/"
  :components ((c->so "anneal-ocl")
               (:file "package")
               (:file "conditions")
               (:file "ffi")
               (:file "gpu-context")
               (:file "image")
               (:file "proximeter")
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
               :fiveam
               :cl-value-noise))
