(in-package :om)

(defvar *fuxcp-sources-dir* nil)
(setf *fuxcp-sources-dir* (make-pathname :directory (append (pathname-directory *load-pathname*) '("sources"))))


(mapc 'compile&load (list
                     (make-pathname :directory (append (pathname-directory *load-pathname*) (list "sources")) :name "package" :type "lisp")
                     (make-pathname :directory (pathname-directory *fuxcp-sources-dir*) :name "fux-counterpoint" :type "lisp")
                     ))


(fill-library '(
    ("ALL" nil nil (fuxcp::fux-cp fuxcp::search-next-fux-cp) nil)
))

(print "FuxCP Loaded")
