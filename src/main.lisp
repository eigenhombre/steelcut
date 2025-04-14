(in-package #:steelcut)

(defparameter +default-features+
  '(:ci :docker :cl-oju))

(defparameter +available-features+
  (append +default-features+
          '(:cmd
            :args
            ;; Future:
            ;; :csv
            ;; :json
            ;; :time
            ;; :webclient
            ;; :webserver
            ;; :xml
            )))

(defun has-feature (feature-name features)
  (find feature-name features))

(defun join-w-sep (sep args)
  (let ((sep-fmt (format nil "~~{~~a~~^~a~~}" sep)))
    (format nil sep-fmt args)))

(defun join/ (&rest args)
  (join-w-sep "/" args))

(defvar *lisp-home* nil
  "If non-nil, overrides the environment variable LISP_HOME.")

(defun lisp-home ()
  (or *lisp-home* (uiop:getenv "LISP_HOME")))

(defun project-path (projname)
  (join/ (lisp-home) projname))

(defun find-project (projname)
  (uiop:probe-file* (project-path projname)))

(project-path "steelcut")
(find-project "steelcut")
(find-project "steelcutter")  ;;=> nil

(defun add-project-file (projname filename contents)
  (let ((path (join/ (project-path projname) filename)))
    (ensure-directories-exist path)
    (spit path contents)))

(defun replace-project (projname s)
  (cl-ppcre:regex-replace-all "PROJNAME" s projname))

(defun render-project-file (projname filename contents)
  (add-project-file projname
                    filename
                    (replace-project projname contents)))

(defun add-gitignore (projname)
  (render-project-file projname
                       ".gitignore"
                       "**/*.fasl
PROJNAME
"))


(defparameter *adopt-setup*
  "
;; Example adapted from https://docs.stevelosh.com/adopt/usage/...
;; See that reference for more details on using Adopt.
(defparameter *ui*
  (adopt:make-interface
   :name \"PROJNAME\"
   :summary \"Do PROJNAME stuff....\"
   :usage \"[OPTIONS]...\"
   :help \"PROJNAME is a program that (...describe me...).\"
   :contents (list
              (adopt:make-option 'version
                                 :long \"version\"
                                 :short #\\V
                                 :help \"Display version and exit.\"
                                 :reduce (constantly t))
              (adopt:make-option 'help
                                 :long \"help\"
                                 :short #\\h
                                 :help \"Display help and exit.\"
                                 :reduce (constantly t)))))

")

(defun adopt-main-fmt (&rest feature-fn-strings)
  (format nil "(defun run (arguments options)
~{~a~}  (format t \"Thanks for using PROJNAME!~~%\"))

(defun main ()
  (handler-case
      (multiple-value-bind (arguments options) (adopt:parse-options *ui*)
        (when (gethash 'help options)
          (adopt:print-help-and-exit *ui*))
        (when (gethash 'version options)
          (format t \"0.0.1~~%\")
          (adopt:exit))
        (run arguments options))
    (error (c)
      (adopt:print-error-and-exit c))))
" feature-fn-strings))

;; The default main, if not using Adopt (:args Steelcut option):
(defun main-fmt (&rest feature-fn-strings)
  (format nil "(defun main ()
~{~a~}  (format t \"Thanks for using PROJNAME!~~%\"))
" feature-fn-strings))

(comment
  (adopt-main-fmt "  (a-form)
" "  3
" "  (z-form
    (inner))
"))

(defmacro str-when (pred s)
  `(if ,pred
       (concatenate 'string ,s (string #\Newline))
       ""))

(defun cmd-example ()
  "(defun cmd-example ()
  (format t \"~a~%\" (cmd:$cmd \"ls\")))
")

(defun cl-oju-example ()
  "(defun cl-oju-example ()
  (println (->> (range 10)
                (take-while (partial #'> 5))
                (drop 2))))
")

(defun add-main-lisp (projname features)
  (render-project-file projname
                       "src/main.lisp"
                       (format nil
                               "(in-package #:PROJNAME)

~a~a~a~a"
                               (str-when (has-feature :cmd features)
                                         (cmd-example))
                               (str-when (has-feature :cl-oju features)
                                         (cl-oju-example))
                               (str-when (has-feature :args features)
                                         *adopt-setup*)
                               ;; Apply either Adopt-style or vanilla main:
                               (funcall (if (has-feature :args features)
                                            #'adopt-main-fmt
                                            #'main-fmt)
                                        (str-when (has-feature :cmd features)
                                                  "  (cmd-example)")
                                        (str-when (has-feature :cl-oju features)
                                                  "  (cl-oju-example)")))))

(defun add-main-package (projname features)
  (render-project-file projname
                       "src/package.lisp"
                       (format nil "(defpackage PROJNAME
  (:use ~{~S~^ ~})
  (:export :main))
"
                               (->> features
                                    deps-for-features
                                    (remove :adopt)
                                    (cons :cl)))))

(defun add-test-lisp (projname)
  (render-project-file projname
                       "test/test.lisp"
                       "(in-package #:PROJNAME.test)

(defun run-tests () (1am:run))

(test example
  (is (equal 2 (+ 1 1))))
"))

(defun add-test-package (projname)
  (render-project-file projname
                       "test/package.lisp"
                       "(defpackage :PROJNAME.test
  (:use :cl :1am)
  (:export :run-tests))
"))

(defun add-dockerfile (projname)
  (render-project-file projname
                       "Dockerfile"
                       "FROM ubuntu

RUN apt-get -qq -y update
RUN apt-get -qq -y upgrade

RUN apt-get install -qq -y sbcl make curl git ecl

# Pull down Quicklisp and install it
RUN curl -s -o quicklisp.lisp http://beta.quicklisp.org/quicklisp.lisp

RUN ecl --load quicklisp.lisp \\
         --eval '(quicklisp-quickstart:install :path \"/home/janice/quicklisp\")' \\
         --eval '(ql:quickload :1am)'

RUN echo | ecl --load /home/janice/quicklisp/setup.lisp --eval '(ql:add-to-init-file)'
RUN echo | sbcl --load /home/janice/quicklisp/setup.lisp --eval '(ql:add-to-init-file)' --quit

ENV LISP_HOME=/home/janice/quicklisp/local-projects
ENV BINDIR=/home/janice/bin
RUN mkdir $BINDIR
WORKDIR /home/janice/PROJNAME

# Run the unit tests:
COPY *.asd *.sh Makefile /home/janice/PROJNAME/
COPY src /home/janice/PROJNAME/src
COPY test /home/janice/PROJNAME/test
RUN make clean test
RUN make
RUN make install
RUN $BINDIR/PROJNAME
"))

(defun add-gha-build (projname)
  (render-project-file projname
                       ".github/workflows/build.yml"
                       "name: Build

on:
  - workflow_dispatch
  - push

jobs:
  build:
    name: build
    runs-on: ubuntu-latest
    steps:
      - name: Checkout repository
        uses: actions/checkout@v2
      - name: Docker build
        run: make docker
"))

(defun add-makefile (projname features)
  (render-project-file projname
                       "Makefile"
                       (format nil ".PHONY: clean install test

PROJNAME: src/*.lisp
	./build.sh

test:
	./test.sh

clean:
	rm -rf PROJNAME

~ainstall: PROJNAME
	test -n \"$(BINDIR)\"  # $$BINDIR
	cp PROJNAME ${BINDIR}
"  (if (or (has-feature :docker features)
           (has-feature :ci features))
       "docker:
	docker build -t PROJNAME .

"
       ""))))

(defun make-executable (path)
  (setf (attributes path) #o755))

(defun add-build-sh (projname)
  (render-project-file projname
                       "build.sh"
                       "#!/bin/sh

# Adapted from
# https://github.com/cicakhq/potato/blob/master/tools/build_binary.sh;
# Quicklisp path hack from
# https://www.darkchestnut.com/2016/quicklisp-load-personal-projects-from-arbitrary-locations/

sbcl --non-interactive \\
     --disable-debugger \\
     --eval '(pushnew (truename \".\") ql:*local-project-directories*)' \\
     --eval '(ql:register-local-projects)' \\
     --eval '(ql:quickload :PROJNAME)' \\
     --eval '(progn (sb-ext:disable-debugger) (sb-ext:save-lisp-and-die \"PROJNAME\" :toplevel #'\"'\"'PROJNAME:main :executable t))'
")
  (make-executable (join/ (project-path projname) "build.sh")))

(defun add-test-sh (projname)
  (render-project-file projname
                       "test.sh"
                       "#!/bin/sh

# Adapted from
# https://github.com/cicakhq/potato/blob/master/tools/build_binary.sh;
# Quicklisp path hack from
# https://www.darkchestnut.com/2016/quicklisp-load-personal-projects-from-arbitrary-locations/

sbcl --non-interactive \\
     --disable-debugger \\
     --eval '(pushnew (truename \".\") ql:*local-project-directories*)' \\
     --eval '(ql:register-local-projects)' \\
     --eval '(ql:quickload :1AM)' \\
     --eval '(ql:quickload :PROJNAME)' \\
     --eval '(asdf:test-system :PROJNAME/test)'
")
  (make-executable (join/ (project-path projname) "test.sh")))

(defun deps-for-features (features)
  (loop for f in features
        for deps = (case f
                     (:args (list :adopt))
                     (:cl-oju (list :arrows :cl-oju))
                     (:cmd (list :cmd))
                     ;; Docker/CI do not add CL deps.
                     )
        append deps))

(defun add-asd (projname features)
  (render-project-file projname
                       (str projname ".asd")
                       (format nil "(defsystem :PROJNAME
  :description \"FIXME\"
  :author \"FIXME\"
  :license \"FIXME\"
  :build-operation \"program-op\"
  :build-pathname \"PROJNAME\"
  :entry-point \"PROJNAME:main\"
  :depends-on (~{~S~^ ~})
  :components ((:module \"src\"
                :components ((:file \"package\")
                             (:file \"main\" :depends-on (\"package\"))))))

(defsystem :PROJNAME/test
  :description \"FIXME\"
  :author \"FIXME\"
  :license \"FIXME\"
  :depends-on (:PROJNAME :1am)
  :serial t
  :components ((:module \"test\"
                :serial t
                :components ((:file \"package\")
                             (:file \"test\"))))
  :perform (asdf:test-op (op system)
                         (funcall (read-from-string \"PROJNAME.test:run-tests\"))))
" (deps-for-features features))))

(defun make-project (projname features)
  (ensure-directories-exist (str (project-path projname) "/"))
  (add-gitignore projname)
  (add-main-lisp projname features)
  (add-test-lisp projname)
  (add-main-package projname features)
  (add-test-package projname)
  (add-makefile projname features)
  (add-build-sh projname)
  (add-test-sh projname)
  (add-asd projname features)
  (when (or (has-feature :ci features)
            (has-feature :docker features))
    (add-dockerfile projname))
  (when (has-feature :ci features)
    (add-gha-build projname))
  t)

(defun file-seq (path)
  (let ((ret))
    (cl-fad:walk-directory path
                           (lambda (name)
                             (push name ret))
                           :directories t)
    ret))

(defun project-files (projname)
  (file-seq (project-path projname)))

(defun project-contents (projname)
  (loop for f in (project-files projname)
        collect (list f (when-not (directory-exists-p f)
                          (slurp f)))))

(defun destroy-project!!! (projname)
  (fad:delete-directory-and-files (project-path projname)))

(defun feature-list-string (features)
  "Helper to format a list of features as a string."
  (if (null features)
      ""
      (join-w-sep ", "
                  (mapcar #'(lambda (x)
                              (string-downcase (format nil "~a" x)))
                          features))))

(defun usage ()
  (format nil
          "Usage: steelcut <appname> [option1 option2 ...]

Available features: ~a

Select with + to include a feature, deselect with -. For example:
  steelcut myapp +cli -docker
  steelcut myapp +cli +json

The default features are: ~a
"
          (feature-list-string +available-features+)
          (feature-list-string +default-features+)))

(defun write-app (appname features)
  (cond
    ((not appname)
     (format t "~a~%"(usage)))

    ((find-project appname)
     (format t "Project ~a already exists!~%" appname))

    (t (progn
         (make-project appname features)
         (format t
                 "Project ~a created (features: ~a).  Thanks for using steelcut!~%"
                 appname
                 (feature-list-string features))))))

;; CLI argument parsing:
(defun starts-with (s c)
  (and (stringp s)
       (> (length s) 0)
       (char= (char s 0) c)))

(defun starts-with-plus? (s)
  (starts-with s #\+))

(defun starts-with-minus? (s)
  (starts-with s #\-))

(defun remove-first-char (s)
  (if (and (stringp s) (> (length s) 0))
      (subseq s 1)
      s))

(defun kw (x)
  (intern (string-upcase x) :keyword))

(defun parse-args (arglist)
  ;; Return (selected . deselected) where
  ;; - selected is a list of arguments starting with #\+ (plus)
  ;;   or plain arguments (no + or - prefix);
  ;; - deselected is a list of arguments starting with #\-.
  (loop with ret = (cons nil nil)
        for arg in arglist
        do (cond
             ;; Handle empty arglist:
             ((null arglist)
              (return ret))
             ;; Handle explicit +:
             ((starts-with-plus? arg)
              (push (kw (remove-first-char arg))
                    (car ret)))
             ;; Handle explicit -:
             ((starts-with-minus? arg)
              (push (kw (remove-first-char arg))
                    (cdr ret)))
             ;; Handle plain arguments (no +/-):
             (t
              (push (kw arg) (car ret))))
        finally (return (cons (nreverse (car ret))
                              (nreverse (cdr ret))))))

(defun supported-feature (kw)
  "Check if the keyword is a supported feature."
  (and (keywordp kw) (find kw +available-features+)))

(defun error-quit (&rest args)
  (apply #'println args)
  (println (usage))
  (uiop:quit 1))

(defun select-args (parsed-args)
  (destructuring-bind (selected . deselected) parsed-args
    (let ((features (copy-list +default-features+)))
      ;; Add selected features:
      (dolist (s selected)
        (when (not (supported-feature (kw s)))
          (error-quit "Unsupported feature:" s))
        (push s features))
      ;; Remove deselected features:
      (dolist (d deselected)
        (when (not (supported-feature (kw d)))
          (error-quit "Unsupported feature:" d))
        (setf features (remove d features :test #'equal)))
      (distinct features))))

(defun main ()
  (let* ((args sb-ext::*posix-argv*)
         (appname (second args))
         (features (select-args (parse-args (cddr args)))))
    (if (find "-h" args :test #'equal)
        (println (usage))
        (write-app appname features))))
