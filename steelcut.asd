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
                             (:file "util" :depends-on ("package"))
                             (:file "main" :depends-on ("package"))))))

(defsystem :steelcut/test
  :description "FIXME"
  :author "FIXME"
  :license "FIXME"
  :depends-on (:steelcut :1am)
  :serial t
  :components ((:module "test"
                :serial t
                :components ((:file "package")
                             (:file "util" :depends-on ("package"))
                             (:file "test"))))
  :perform (asdf:test-op (op system)
                         (funcall (read-from-string "steelcut.test:run-tests"))))
