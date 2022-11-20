(in-package #:steelcut)

(defun lisp-home () (uiop:getenv "LISP_HOME"))

(defun join-w-sep (sep args)
  (let ((sep-fmt (format nil "~~{~~a~~^~a~~}" sep)))
    (format nil sep-fmt args)))

(defun join (&rest args)
  (join-w-sep "/" args))

(defun project-path (projname)
  (join (lisp-home) projname))

(defun find-project (projname)
  (uiop:probe-file* (project-path projname)))

(project-path "steelcut")
(find-project "steelcut")
(find-project "steelcutter")

(project-path "steelcut")

(defun make-project (projname)
  (ensure-directories-exist (str (project-path projname) "/")))

(make-project "foo")
(project-path "foo")
(find-project "foo")
(find-project "food")

(defun destroy-project!!! (projname)
  (fad:delete-directory-and-files (project-path projname)))

(comment
 (destroy-project!!! "foo")
 (find-project "foo"))

(defun add-project-file (projname filename contents)
  (spit (join (project-path projname) filename) contents))

(defun add-gitignore (projname)
  (add-project-file projname ".gitignore"
                    (format nil "**/*.fasl
~a
"
                            projname)))

(add-gitignore "foo")

(slurp (join (project-path "foo") ".gitignore"))

(defun main ()
  (format t "Thanks for using steelcut!~%"))
