(in-package #:steelcut)

(defun lisp-home () (uiop:getenv "LISP_HOME"))

(defun str (&rest args)
  (format nil "~{~a~}" args))

(defun project-path (projname)
  (str (lisp-home) "/" projname))

(defun find-project (projname)
  (uiop:probe-file* (project-path projname)))

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

(destroy-project!!! "foo")
(find-project "foo")

(defun main ()
  (format t "Thanks for using steelcut!~%"))
