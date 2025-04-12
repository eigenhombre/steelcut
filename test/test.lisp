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
                      (lisp-home)))))))

(test test-no-arguments-generates-usage-string
  (is (search "Usage: steelcut"
              (with-out-str
                (write-app nil +default-features+)))))

(test temp-smoke-test-app
  (with-temporary-dir (d)
    (with-testing-lisp-home ((namestring d))
      (is (search "testingapp created"
                  (with-out-str
                    (write-app "testingapp" +default-features+))))
      (is (search "testingapp already exists"
                  (with-out-str
                    (write-app "testingapp" +default-features+)))))))

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

(defmacro with-setup (appsym appname appdirsym depssym features &body body)
  "
  Test harness providing a temporary target directory for building or
  testing an application.

  Arguments:
    APPSYM     – Symbol to bind to the name of the application
                 (which is usually a string).
    APPNAME    – The actual name of the application (a string literal
                 or expression).
    APPDIRSYM  – Symbol to bind to the generated application directory
                 path.
    DEPSSYM    – Symbol naming a function (with no arguments) that
                 returns the app's dependencies based on its .asd
                 file. If DEPSSYM is the symbol _, the dependency
                 logic is omitted.
    FEATURES   – A list of features to pass to `write-app`.
    BODY       – The test body to run inside the setup context, with
                 all symbols bound as above.

  Example usage:
    (with-setup app \"demo\" appdir deps +default-features+
      (is (uiop:file-exists-p (join/ appdir \"Makefile\"))))

    ;; If deps is unused, pass _ to skip the binding and suppress compiler warnings:
    (with-setup app \"demo\" appdir _ +default-features+
      (format t \"App dir is ~a~%\" appdir))


  Test harness providing temporary target directory.
  If DEPSSYM is _, it is omitted entirely.
  "
  (let ((featuresym (gensym))
        (d (gensym)))
    `(let ((,appsym ,appname)
           (,featuresym ,features))
       (with-temporary-dir (,d)
         (with-testing-lisp-home ((namestring ,d))
           (let ((,appdirsym (merge-pathnames ,appsym ,d)))
             ,@(if (eq depssym '_)
                   ;; skip defining deps function entirely
                   `((no-output
                       (write-app ,appsym ,featuresym))
                     ,@body)
                   ;; include deps function
                   `((labels ((,depssym ()
                                (find-deps-in-asd-string
                                 (read-from-string
                                  (slurp (join/ ,appdirsym (str ,appsym ".asd")))))))
                       (no-output
                         (write-app ,appsym ,featuresym))
                       ,@body)))))))))

(defun has-cmd-example-p (source)
  (cl-ppcre:scan "(?i)\\(\\s*defun\\s+cmd-example\\b" source))

(defun has-cl-oju-example-p (source)
  (cl-ppcre:scan "(?i)\\(\\s*defun\\s+cl-oju-example\\b" source))

(defun has-make-docker-target-p (source)
  (cl-ppcre:scan "(?i)docker:" source))

(test needed-files-created
  (with-setup appname "testingapp" appdir _ +default-features+
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
             (is (uiop:file-exists-p (join/ appdir file))))))

(test deselecting-ci-feature-turns-off-github-action-file
  (with-setup appname "testingapp" appdir _ (remove :ci +default-features+)
    (let ((file ".github/workflows/build.yml"))
      (is (not (uiop:file-exists-p (join/ appdir file)))))))

(test deselecting-ci-and-docker-turns-off-docker
  (with-setup appname "testingapp" appdir _ (remove :ci
                                                    (remove :docker +default-features+))
    (let ((make-contents (slurp (join/ appdir "Makefile"))))
      (is (not (uiop:file-exists-p (join/ appdir "Dockerfile"))))
      (is (not (has-make-docker-target-p make-contents))))))

(defun find-deps-in-asd-string (raw-asd)
  (second (drop-while (lambda (x) (not (equal x :depends-on)))
                      raw-asd)))

(test cmd-feature-adds-dependency-and-example
  (with-setup appname "testingapp" appdir deps +default-features+
    (let ((main-text (slurp (join/ appdir "src/main.lisp"))))
      ;; :cmd is not there:
      (is (not (find :cmd (deps))))
      ;; It doesn't contain the example function:
      (is (not (has-cmd-example-p main-text)))))

  ;; Add :cmd to features, should get the new dependency:
  (with-setup appname "test2" appdir deps (cons :cmd +default-features+)
    (let ((main-text (slurp (join/ appdir "src/main.lisp"))))
      ;; :cmd is now there:
      (is (find :cmd (deps)))
      ;; It contains the example function:
      (is (has-cmd-example-p main-text)))))

(test cl-oju-feature
  (with-setup appname "testingapp" appdir deps +default-features+
    (let ((main-text (slurp (join/ appdir "src/main.lisp"))))
      ;; :cl-oju is there:
      (is (find :cl-oju (deps)))
      ;; It contains the example function:
      (is (has-cl-oju-example-p main-text))))
  (with-setup appname "testingapp" appdir deps (remove :cl-oju +default-features+)
    (let ((main-text (slurp (join/ appdir "src/main.lisp"))))
      ;; :cl-oju is NOT there:
      (is (not (find :cl-oju (deps))))
      ;; It contains the example function:
      (is (not (has-cl-oju-example-p main-text))))))
