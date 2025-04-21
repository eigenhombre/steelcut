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
      (is (member :cl-oju (deps))))

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
                       (assert (= 0 (write-app ,appsym ,featuresym))))
                     ,@body)
                   ;; include deps function
                   `((labels ((,depssym ()
                                (find-deps-in-asd-string
                                 (read-from-string
                                  (slurp (join/ ,appdirsym (str ,appsym ".asd")))))))
                       (no-output
                         (assert (= 0 (write-app ,appsym ,featuresym))))
                       ,@body)))))))))

(defun has-cmd-example-p (source)
  (cl-ppcre:scan "(?i)\\(\\s*defun\\s+cmd-example\\b" source))

(defun has-cl-oju-example-p (source)
  (cl-ppcre:scan "(?i)\\(\\s*defun\\s+cl-oju-example\\b" source))

(defun has-adopt-example-p (source)
  (cl-ppcre:scan "(?i)adopt\:print-help-and-exit\\b" source))

(defun has-make-docker-target-p (source)
  (cl-ppcre:scan "(?i)docker:" source))

(defun has-yaml-example-p (source)
  (cl-ppcre:scan "(?i)\\(\\s*defun\\s+yaml-example\\b" source))

(defun has-csv-example-p (source)
  (cl-ppcre:scan "(?i)\\(\\s*defun\\s+csv-example\\b" source))

(defun has-json-example-p (source)
  (cl-ppcre:scan "(?i)\\(\\s*defun\\s+json-example\\b" source))

(test cannot-create-project-whose-name-is-a-feature-name
  (loop for feat in +default-features+
        do (with-temporary-dir (d)
             (with-testing-lisp-home ((namestring d))
               (no-output
                 (is (not= 0 (write-app (string-downcase (symbol-name feat))
                                        +default-features+))))))))

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

(test feature-deps-and-examples
  (loop for (feature deplist fn-check) in
        '((:args   (:adopt)          has-adopt-example-p)
          (:cmd    (:cmd)            has-cmd-example-p)
          (:cl-oju (:arrows :cl-oju) has-cl-oju-example-p)
          (:csv    (:cl-csv)         has-csv-example-p)
          (:json   (:cl-json)        has-json-example-p)
          (:yaml   (:cl-yaml)        has-yaml-example-p))
        do
           (with-setup appname "testingapp" appdir deps (remove feature +default-features+)
             (let ((main-text (slurp (join/ appdir "src/main.lisp"))))
               ;; The associated dependency is NOT there:
               (loop for dep in deplist
                     do (is (not (member dep (deps)))))
               ;; It does NOT contain the example function:
               (is (not (funcall fn-check main-text)))))
           (with-setup appname "testingapp" appdir deps (cons feature +default-features+)
             (let ((main-text (slurp (join/ appdir "src/main.lisp"))))
               ;; The associated dependency is now there:
               (loop for dep in deplist
                     do (is (member dep (deps))))
               ;; It contains the example function:
               (is (funcall fn-check main-text))))))
