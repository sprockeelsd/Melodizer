(in-package :om)

(defvar *FuxCP-path* (make-pathname  :directory (append (pathname-directory *load-pathname*) (list "FuxCP"))))

(require-library "GIL")

(defpackage :fuxcp
(:use "COMMON-LISP" "OM" "CL-USER"))
