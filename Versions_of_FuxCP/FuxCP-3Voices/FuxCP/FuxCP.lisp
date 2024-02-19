(in-package :om)

(defvar *fuxcp-sources-dir* nil)
(setf *fuxcp-sources-dir* (make-pathname :directory (append (pathname-directory *load-pathname*) '("sources"))))

(mapc 'compile&load (list
    (make-pathname :directory (pathname-directory *fuxcp-sources-dir*) :name "package" :type "lisp")
    (make-pathname :directory (pathname-directory *fuxcp-sources-dir*) :name "utils" :type "lisp")
    (make-pathname :directory (pathname-directory *fuxcp-sources-dir*) :name "constraints" :type "lisp")
    (make-pathname :directory (pathname-directory *fuxcp-sources-dir*) :name "1sp-ctp" :type "lisp")
    (make-pathname :directory (pathname-directory *fuxcp-sources-dir*) :name "2sp-ctp" :type "lisp")
    (make-pathname :directory (pathname-directory *fuxcp-sources-dir*) :name "3sp-ctp" :type "lisp")
    (make-pathname :directory (pathname-directory *fuxcp-sources-dir*) :name "4sp-ctp" :type "lisp")
    (make-pathname :directory (pathname-directory *fuxcp-sources-dir*) :name "5sp-ctp" :type "lisp")
    (make-pathname :directory (pathname-directory *fuxcp-sources-dir*) :name "3v-ctp" :type "lisp")
    (make-pathname :directory (pathname-directory *fuxcp-sources-dir*) :name "cf" :type "lisp")
    (make-pathname :directory (pathname-directory *fuxcp-sources-dir*) :name "fuxcp-main" :type "lisp")
    (make-pathname :directory (pathname-directory *fuxcp-sources-dir*) :name "interface" :type "lisp")
)) 


(fill-library '(
    ("Solver" nil (fuxcp::cp-params) nil)
))

(print "FuxCP Loaded")