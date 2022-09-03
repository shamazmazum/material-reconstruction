(in-package :asdf-user)

(defclass no-load-source (source-file)
  ())

(defmethod perform ((operation load-op) (component no-load-source))
  t)

(defclass c-library (no-load-source)
  ()
  (:default-initargs
   :type "c"))

(defmethod output-files ((operation compile-op) (component c-library))
  (values
   (list
    (make-pathname :name (concatenate
                          'string "lib"
                          (pathname-name (component-pathname component)))
                   :type #-darwin "so" #+darwin "dylib"
                   :directory (pathname-directory (component-pathname component))))
   t))

(defmethod perform ((operation compile-op) (component c-library))
  (flet ((nn (x) (uiop:native-namestring x)))
    (let ((source-name (component-pathname component))
          (lib-name    (car (output-files operation component))))
      (uiop:run-program
       (list "cc" "-O3" "-fPIC" "-shared" "-I/usr/local/include" "-L/usr/local/lib"
             "-o"
             (nn lib-name)
             (nn source-name)
             "-lm" "-lOpenCL" "-lfftw3")))))
