(defsystem :steelcut
  :description "Common Lisp Project Generator"
  :author "John Jacobsen"
  :license "MIT"
  :build-operation "program-op"
  :build-pathname "steelcut"
  :entry-point "steelcut:main"
  :depends-on (:arrows
               :cl-fad
               :cl-oju
               :file-attributes
               :str)
  :components ((:module "src"
                :components ((:file "package")
                             (:file "main" :depends-on ("package"))))))

(defsystem :steelcut/test
  :description "FIXME"
  :author "FIXME"
  :license "FIXME"
  :depends-on (:arrows :steelcut :1am)
  :serial t
  :components ((:module "test"
                :serial t
                :components ((:file "package")
                             (:file "test" :depends-on ("package")))))
  :perform (asdf:test-op (op system)
                         (funcall (read-from-string "steelcut.test:run-tests"))))
