(in-package #:steelcut.test)

(defun run-tests () (1am:run))

(defun rmdir (path)
  (uiop:delete-directory-tree (pathname path) :validate t))

(defun tmpdir ()
  "
  Generate a temporary directory within the system-specified
  temporary dirs.
  "
  (labels ((bit-shift-left (n)
             (ash 1 n)))
    (make-pathname :directory
                   (list :absolute
                         (format nil "~a~a"
                                 (uiop:temporary-directory)
                                 (random (1- (bit-shift-left 64))))))))

(defmacro with-temporary-dir ((dir) &body body)
  "
  Execute body within a temporary directory whose pathname
  is bound to `dir`.
  "
  `(let* ((,dir (ensure-directories-exist (tmpdir))))
     (unwind-protect (progn ,@body)
       (rmdir ,dir))))

(defmacro with-testing-lisp-home ((dir) &body body)
  "
  Bind *lisp-home* within a different (probably temporary
  directory.
  "
  `(let ((steelcut::*lisp-home* ,dir))
     ,@body))

(test file-in-temporary-directory-exists
  (with-temporary-dir (dir)
    (let ((file (merge-pathnames "test.txt" dir)))
      (with-open-file (stream file :direction :output :if-exists :supersede)
        (format stream "Hello, temporary world!"))
      ;; Verify the file exists
      (is (uiop:file-exists-p file)))))

(test temp-lisp-home-is-rebound
  (with-temporary-dir (d)
    (with-testing-lisp-home ((namestring d))
      (is (not (equal (uiop:getenv "LISP_HOME")
                      (steelcut::lisp-home)))))))

(test test-no-arguments-generates-usage-string
  (is (search "Usage: steelcut"
              (with-out-str
                (steelcut::write-app nil steelcut::+default-features+)))))

(test temp-smoke-test-app
  (with-temporary-dir (d)
    (with-testing-lisp-home ((namestring d))
      (is (search "testingapp created."
                  (with-out-str
                    (steelcut::write-app "testingapp" steelcut::+default-features+))))
      (is (search "testingapp already exists"
                  (with-out-str
                    (steelcut::write-app "testingapp" steelcut::+default-features+)))))))

(test needed-files-created
  (let ((appname "testingapp"))
    (with-temporary-dir (d)
      (with-testing-lisp-home ((namestring d))
        (with-out-str
          (steelcut::write-app appname steelcut::+default-features+))
        (loop for file in '("Makefile"
                            "Dockerfile"
                            "build.sh"
                            "test.sh"
                            "src/main.lisp"
                            "src/package.lisp"
                            "test/test.lisp"
                            "test/package.lisp")
              do
                 (let ((appdir (merge-pathnames appname d)))
                   (is (uiop:file-exists-p (steelcut::join/ appdir file)))))))))
