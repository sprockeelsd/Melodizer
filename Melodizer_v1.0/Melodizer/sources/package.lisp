(in-package :om)

(defvar *MELODIZER-path* (make-pathname  :directory (append (pathname-directory *load-pathname*) (list "MELODIZER"))))

(require-library "GIL")

(defpackage :mldz
(:use "COMMON-LISP" "OM" "CL-USER"))

