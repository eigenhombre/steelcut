(in-package #:steelcut)

(defun lisp-home () (uiop:getenv "LISP_HOME"))

(defun join-w-sep (sep args)
  (let ((sep-fmt (format nil "~~{~~a~~^~a~~}" sep)))
    (format nil sep-fmt args)))

(defun join/ (&rest args)
  (join-w-sep "/" args))

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
  (replace-all "PROJNAME" projname s))

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

(defun add-main-lisp (projname)
  (render-project-file projname
                       "src/main.lisp"
                       "(in-package #:PROJNAME)

(defun main ()
  (format t \"Thanks for using PROJNAME!~%\"))
"))

(defun add-main-package (projname)
  (render-project-file projname
                       "src/package.lisp"
                       "(defpackage PROJNAME
  (:use :cl :arrows)
  (:export :main))
"))

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

(defun add-makefile (projname)
  (render-project-file projname
                       "Makefile"
                       ".PHONY: clean install test

PROJNAME: src/*.lisp
	./build.sh

test:
	./test.sh

clean:
	rm -rf PROJNAME

docker:
	docker build -t PROJNAME .

install: PROJNAME
	test -n \"$(BINDIR)\"  # $$BINDIR
	cp PROJNAME ${BINDIR}
"))

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

(defun add-asd (projname)
  (render-project-file projname
                       (str projname ".asd")
                       "(defsystem :PROJNAME
  :description \"FIXME\"
  :author \"FIXME\"
  :license \"FIXME\"
  :build-operation \"program-op\"
  :build-pathname \"PROJNAME\"
  :entry-point \"PROJNAME:main\"
  :depends-on (:arrows)
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
"))

(defun make-project (projname)
  (ensure-directories-exist (str (project-path projname) "/"))
  (add-gitignore projname)
  (add-main-lisp projname)
  (add-test-lisp projname)
  (add-main-package projname)
  (add-test-package projname)
  (add-makefile projname)
  (add-build-sh projname)
  (add-test-sh projname)
  (add-asd projname)
  (add-dockerfile projname)
  (add-gha-build projname)
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

(comment
 (destroy-project!!! "foo")
 (make-project "foo")

 (project-path "foo")
 (find-project "foo")
 (find-project "foo")
 (project-contents "foo"))

(defun usage ()
  "Usage: steelcut <appname>")

(defun main ()
  (let* ((args sb-ext::*posix-argv*)
         (appname (second args)))
    (cond
      ((not appname)
       (format t "~a~%"(usage)))

      ((find-project appname)
       (format t "Project ~a already exists!~%" appname))

      (t (progn
           (make-project appname)
           (format t
                   "Project ~a created.  Thanks for using steelcut!~%"
                   appname))))))
