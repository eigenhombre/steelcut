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
      (is (search "testingapp created"
                  (with-out-str
                    (steelcut::write-app "testingapp" steelcut::+default-features+))))
      (is (search "testingapp already exists"
                  (with-out-str
                    (steelcut::write-app "testingapp" steelcut::+default-features+)))))))

(defmacro with-setup (d appsym appname features &body body)
  "Test harness providing temporary target directory"
  (let ((featuresym (gensym)))
    `(let ((,appsym ,appname)
           (,featuresym ,features))
       (with-temporary-dir (,d)
         (with-testing-lisp-home ((namestring d))
           (no-output
             (steelcut::write-app ,appsym ,featuresym))
           ,@body)))))

(defmacro no-output (&body body)
  `(with-out-str ,@body))

(test parsing-arguments
  (is (equal (steelcut::parse-args ())
             (cons nil nil)))
  (is (equal (steelcut::parse-args '("a"))
             (cons '(:a) nil)))
  (is (equal (steelcut::parse-args '("a" "-b"))
             (cons '(:a) '(:b))))
  (is (equal (steelcut::parse-args '("+a" "-b"))
             (cons '(:a) '(:b))))
  (is (equal (steelcut::parse-args '("+a" "-b" "c" "-d" "+e"))
             (cons '(:a :c :e) '(:b :d)))))

(test needed-files-created
  (with-setup d appname "testingapp" steelcut::+default-features+
    (loop for file in '("Makefile"
                        "Dockerfile"
                        ".github/workflows/build.yml"
                        "build.sh"
                        "test.sh"
                        "src/main.lisp"
                        "src/package.lisp"
                        "test/test.lisp"
                        "test/package.lisp")
          do
             (let ((appdir (merge-pathnames appname d)))
               (is (uiop:file-exists-p (steelcut::join/ appdir file)))))))

(defun has-cmd-example-p (source)
  (cl-ppcre:scan "(?i)\\(\\s*defun\\s+cmd-example\\b" source))

(defun has-cl-oju-example-p (source)
  (cl-ppcre:scan "(?i)\\(\\s*defun\\s+cl-oju-example\\b" source))

(defun has-make-docker-target-p (source)
  (cl-ppcre:scan "(?i)docker:" source))

(test deselecting-ci-feature-turns-off-github-action-file
  (with-setup d appname "testingapp" (remove :ci steelcut::+default-features+)
    (let ((file ".github/workflows/build.yml")
          (appdir (merge-pathnames appname d)))
      (is (not (uiop:file-exists-p (steelcut::join/ appdir file)))))))

(test deselecting-ci-and-docker-turns-off-docker
  (with-setup d appname "testingapp" (remove :ci
                                             (remove :docker steelcut::+default-features+))
    (let* ((appdir (merge-pathnames appname d))
           (make-contents (slurp (steelcut::join/ appdir "Makefile"))))
      (is (not (uiop:file-exists-p (steelcut::join/ appdir "Dockerfile"))))
      (is (not (has-make-docker-target-p make-contents))))))

(defun find-deps-in-asd-string (raw-asd)
  (second (drop-while (lambda (x) (not (equal x :depends-on)))
                      raw-asd)))

(test cmd-feature-adds-dependency-and-example
  (with-setup d appname "testingapp" steelcut::+default-features+
    (let* ((appdir (merge-pathnames appname d))
           (deps
             (find-deps-in-asd-string
              (read-from-string
               (slurp (steelcut::join/ appdir "testingapp.asd")))))
           (main-text (slurp (steelcut::join/ appdir "src/main.lisp"))))
      ;; :cmd is not there:
      (is (not (find :cmd deps)))
      ;; It doesn't contain the example function:
      (is (not (has-cmd-example-p main-text)))))

  ;; Add :cmd to features, should get the new dependency:
  (with-setup d appname "test2" (cons :cmd steelcut::+default-features+)
    (let* ((appdir (merge-pathnames appname d))
           (deps
             (find-deps-in-asd-string
              (read-from-string
               (slurp (steelcut::join/ appdir "test2.asd")))))
           (main-text (slurp (steelcut::join/ appdir "src/main.lisp"))))
      ;; :cmd is now there:
      (is (find :cmd deps))
      ;; It contains the example function:
      (is (has-cmd-example-p main-text)))))

(test cl-oju-feature
  (with-setup d appname "testingapp" steelcut::+default-features+
    (let* ((appdir (merge-pathnames appname d))
           (deps
             (find-deps-in-asd-string
              (read-from-string
               (slurp (steelcut::join/ appdir "testingapp.asd")))))
           (main-text (slurp (steelcut::join/ appdir "src/main.lisp"))))
      ;; :cl-oju is there:
      (is (find :cl-oju deps))
      ;; It contains the example function:
      (is (has-cl-oju-example-p main-text))))
  (with-setup d appname "testingapp" (remove :cl-oju steelcut::+default-features+)
    (let* ((appdir (merge-pathnames appname d))
           (deps
             (find-deps-in-asd-string
              (read-from-string
               (slurp (steelcut::join/ appdir "testingapp.asd")))))
           (main-text (slurp (steelcut::join/ appdir "src/main.lisp"))))
      ;; :cl-oju is NOT there:
      (is (not (find :cl-oju deps)))
      ;; It contains the example function:
      (is (not (has-cl-oju-example-p main-text))))))
