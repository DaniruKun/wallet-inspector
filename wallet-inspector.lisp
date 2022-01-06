(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload '(:unix-opts :jsown)))

(defpackage :wallet-inspector
  (:use :cl)
  (:export :toplevel))

(in-package :wallet-inspector)

(opts:define-opts
  (:name :help
   :description "Print this help text"
   :short #\h
   :long "help"))

(defun toplevel ()
  (multiple-value-bind (options free-args)
	(opts:get-opts)
  (if (getf options :help)
	  (progn
		(opts:describe
		 :prefix "You are in my app. Usage:"
		 :args "[keywords]")
		(uiop:quit)))
  (if (getf options :nb) (format t "NB"))))
