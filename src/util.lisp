(in-package #:steelcut)

(defun join-w-sep (sep args)
  (let ((sep-fmt (format nil "~~{~~a~~^~a~~}" sep)))
    (format nil sep-fmt args)))

(defun join/ (&rest args)
  (join-w-sep "/" args))
